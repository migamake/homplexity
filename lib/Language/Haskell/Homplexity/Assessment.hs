{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Main module parsing inputs, and running analysis.
module Language.Haskell.Homplexity.Assessment (
    metrics
  --, measureAllOccurs
  ) where

import Data.Data
import Data.Monoid

import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Cyclomatic
import Language.Haskell.Homplexity.Message
import Language.Haskell.Homplexity.Metric
import Language.Haskell.Homplexity.TypeComplexity

import HFlags

{-
numFunctions = length
             . filter isFunBind
             . getModuleDecls

testNumFunctions = (>20)

numFunctionsMsg = "More than 20 functions per module"

numFunctionsSeverity = Warning
 -}

-- * Showing metric measurements
measureAll :: Metric m c => Assessment m -> (a -> [c]) -> Proxy m -> Proxy c -> a -> Log
measureAll assess generator metricType fragType = mconcat
                                                . map       (warnOfMeasure assess metricType fragType)
                                                . generator

measureTopOccurs :: (Data from, Metric m c) => Assessment m -> Proxy m -> Proxy c -> from -> Log
measureTopOccurs assess = measureAll assess occurs

--measureAllOccurs  :: (CodeFragment c, Metric m c) => Severity -> Proxy m -> Proxy c -> Program -> Log
-- | Measure all occurences of a given @CodeFragment@ with a given @Metric@,
-- then use @Assessment@ on them and give a list of @Log@ messages.
--
-- Arguments come in the following order:
-- 1. @Assessment@ for the value of the @Metric@.
-- 2. @Metric@ given as @Proxy@ type.
-- 3. @CodeFragment@ given as @Proxy@ type.
-- 4. Program containing @CodeFragment@s.
measureAllOccurs :: (Data from, Metric m c) => Assessment m -> Proxy m -> Proxy c -> from -> Log
measureAllOccurs assess = measureAll assess allOccurs

-- | Type of functions that convert a @Metric@ into a log message.
type Assessment m = m -> (Severity, String)

warnOfMeasure :: (CodeFragment c, Metric m c) => Assessment m -> Proxy m -> Proxy c -> c -> Log
warnOfMeasure assess metricType fragType c = message  severity
                                                     (        fragmentLoc  c )
                                                     (unwords [fragmentName c
                                                              ,"has"
                                                              ,show result
                                                              ,recommendation])
  where
    (severity, recommendation) = assess result
    result = measureFor metricType fragType c

-- * Assessments of severity for used @Metric@s.
-- ** Module definition checks
defineFlag "moduleLinesWarning"  (500  :: Int) "issue warning when module exceeds this number of lines"
defineFlag "moduleLinesCritical" (3000 :: Int) "issue critical when module exceeds this number of lines"

assessModuleLength :: Assessment LOC
assessModuleLength (fromIntegral -> locs)
                   | locs > flags_moduleLinesWarning  = (Warning,  "should be kept below "        ++
                                                                    show flags_moduleLinesWarning ++
                                                                   " lines of code.")
                   | locs > flags_moduleLinesCritical = (Critical, "this function exceeds "       ++
                                                                    show flags_moduleLinesCritical ++
                                                                   " lines of code.")
                   | otherwise    = (Info,     ""                                        )

-- ** Function definition checks
-- *** Number of lines of code within function body
defineFlag "functionLinesWarning"  (20 :: Int) "issue warning when function exceeds this number of lines"
defineFlag "functionLinesCritical" (40 :: Int) "issue critical when function exceeds this number of lines"

assessFunctionLength :: Assessment LOC
assessFunctionLength (fromIntegral -> locs)
                   | locs > flags_functionLinesWarning  = (Warning,  "should be kept below "          ++
                                                                       show flags_functionLinesWarning ++
                                                                      " lines of code.")
                   | locs > flags_functionLinesCritical = (Critical, "this function exceeds "          ++
                                                                       show flags_functionLinesCritical ++
                                                                      " lines of code.")
                   | otherwise                          = (Info,     ""                                 )


-- *** Decision depth of function definition
defineFlag "functionDepthWarning"  (4 :: Int) "issue warning when function exceeds this decision depth"
defineFlag "functionDepthCritical" (8 :: Int) "issue critical when function exceeds this decision depth"

assessFunctionDepth :: Assessment Depth
assessFunctionDepth (fromIntegral -> depth)
                    | depth > flags_functionDepthWarning = (Warning,  "should have no more than " ++
                                                                       show depth                 ++
                                                                      " nested conditionals"            )
                    | depth > flags_functionDepthWarning = (Critical, "should never exceed " ++
                                                                       show depth            ++
                                                                      " nesting levels for conditionals")
                    | otherwise = (Info,    ""                                )

-- *** Cyclomatic complexity of function definition
defineFlag "functionCCWarning"  (20::Int) "issue warning when function's cyclomatic complexity exceeds this number"
defineFlag "functionCCCritical" (50::Int) "issue critical when function's cyclomatic complexity exceeds this number"

assessFunctionCC :: Assessment Cyclomatic
assessFunctionCC (fromIntegral -> cy)
                 | cy > flags_functionCCWarning  = (Warning,  "should be less than "        ++
                                                               show flags_functionCCWarning)
                 | cy > flags_functionCCCritical = (Critical, "must never be as high as " ++
                                                               show flags_functionCCCritical)
                 | otherwise                     = (Info,     ""                               )

-- ** Type signature complexity
-- *** Type constructor depth in each type signature
defineFlag "typeConDepthWarning"  (6::Int) "issue warning when type constructor depth exceeds this number"
defineFlag "typeConDepthCritical" (9::Int) "issue critical when type constructor depth exceeds this number"

assessTypeConDepth :: Assessment ConDepth
assessTypeConDepth (fromIntegral -> cy)
                 | cy > flags_typeConDepthWarning  = (Warning,  "should be less than "        ++
                                                                 show flags_typeConDepthWarning )
                 | cy > flags_typeConDepthCritical = (Critical, "must never be as high as " ++
                                                                 show flags_typeConDepthCritical)
                 | otherwise                       = (Info,     ""                              )

-- *** Number of function arguments mentioned in each type signature
defineFlag "numFunArgsWarning"  (5::Int) "issue warning when number of function arguments exceeds this number"
defineFlag "numFunArgsCritical" (9::Int) "issue critical when number of function arguments exceeds this number"

assessNumFunArgs :: Assessment NumFunArgs
assessNumFunArgs (fromIntegral -> cy)
                 | cy > flags_numFunArgsWarning  = (Warning,  "should be less than " ++ show flags_numFunArgsWarning )
                 | cy > flags_numFunArgsCritical = (Critical, "must never reach "    ++ show flags_numFunArgsCritical)
                 | otherwise                     = (Info,     ""                                                     )

-- * Computing and assessing @Metric@s for all @CodeFragment@.
-- | Compute all metrics, and assign severity depending on configured thresholds.
metrics :: [Program -> Log]
metrics  = [measureTopOccurs assessModuleLength   locT        moduleT
           ,measureTopOccurs assessFunctionLength locT        functionT
           ,measureTopOccurs assessFunctionDepth  depthT      functionT
           ,measureTopOccurs assessFunctionCC     cyclomaticT functionT
           ,measureTopOccurs assessTypeConDepth   conDepthT   typeSignatureT
           ,measureTopOccurs assessNumFunArgs     numFunArgsT typeSignatureT]

