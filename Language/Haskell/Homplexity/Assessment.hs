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
module Language.Haskell.Homplexity.Assessment (metrics) where

import Data.Data
import Data.List
import Control.Monad

import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Cyclomatic
import Language.Haskell.Homplexity.Message
import Language.Haskell.Homplexity.Metric
import Language.Haskell.Homplexity.Parse
import Language.Haskell.Homplexity.TypeComplexity
import System.Directory
import System.FilePath
import System.IO

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
--measureAll  :: (CodeFragment c, Metric m c) => Severity -> (Program -> [c]) -> Proxy m -> Proxy c -> Program -> Log
measureAll :: Metric m c =>Assessment m -> (a -> [c]) -> Proxy m -> Proxy c -> a -> Log
measureAll assess generator metricType fragType = mconcat
                                                . map       (warnOfMeasure assess metricType fragType)
                                                . generator

--measureTopOccurs  :: (CodeFragment c, Metric m c) => Severity -> Proxy m -> Proxy c -> Program -> Log
measureTopOccurs :: (Data from, Metric m c) =>Assessment m -> Proxy m -> Proxy c -> from -> Log
measureTopOccurs assess = measureAll assess occurs

--measureAllOccurs  :: (CodeFragment c, Metric m c) => Severity -> Proxy m -> Proxy c -> Program -> Log
measureAllOccurs :: (Data from, Metric m c) =>Assessment m -> Proxy m -> Proxy c -> from -> Log
measureAllOccurs assess = measureAll assess allOccurs

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

defineFlag "functionLinesWarning"  (20 :: Int) "issue warning when function exceeds this number of lines"
defineFlag "functionLinesCritical" (40 :: Int) "issue critical when function exceeds this number of lines"

assessFunctionLength :: Assessment LOC
assessFunctionLength (fromIntegral -> lines)
                   | lines > flags_functionLinesWarning  = (Warning,  "should be kept below "          ++
                                                                       show flags_functionLinesWarning ++
                                                                      " lines of code.")
                   | lines > flags_functionLinesCritical = (Critical, "this function exceeds "          ++
                                                                       show flags_functionLinesCritical ++
                                                                      " lines of code.")
                   | otherwise                           = (Info,     ""                                 )


defineFlag "moduleLinesWarning"  (500  :: Int) "issue warning when module exceeds this number of lines"
defineFlag "moduleLinesCritical" (3000 :: Int) "issue critical when module exceeds this number of lines"

assessModuleLength :: Assessment LOC
assessModuleLength (fromIntegral -> lines)
                   | lines > flags_moduleLinesWarning  = (Warning,  "should be kept below "        ++
                                                                     show flags_moduleLinesWarning ++
                                                                    " lines of code.")
                   | lines > flags_moduleLinesCritical = (Critical, "this function exceeds "       ++
                                                                     show flags_moduleLinesCritical ++
                                                                    " lines of code.")
                   | otherwise    = (Info,     ""                                         )

defineFlag "functionDepthWarning"  (4 :: Int) "issue warning when function exceeds this decision depth"
defineFlag "functionDepthCritical" (8 :: Int) "issue critical when function exceeds this decision depth"

assessFunctionDepth :: Assessment Depth
assessFunctionDepth (fromIntegral -> depth)
                    | depth > flags_functionDepthWarning = (Warning, "should have no more than " ++
                                                                      show depth                 ++
                                                                     " nested conditionals"            )
                    | depth > flags_functionDepthWarning = (Warning, "should never exceed " ++
                                                                      show depth            ++
                                                                     " nesting levels for conditionals")
                    | otherwise = (Info,    ""                                )

defineFlag "functionCCWarning"  (20::Int) "issue warning when function's cyclomatic complexity exceeds this number"
defineFlag "functionCCCritical" (50::Int) "issue critical when function's cyclomatic complexity exceeds this number"

assessFunctionCC :: Assessment Cyclomatic
assessFunctionCC (fromIntegral -> cy)
                 | cy > flags_functionCCWarning  = (Warning, "should be less than "        ++
                                                              show flags_functionCCWarning)
                 | cy > flags_functionCCCritical = (Warning, "must never be as high as " ++
                                                              show flags_functionCCCritical)
                 | otherwise                     = (Info,    ""                               )

defineFlag "typeConDepthWarning"  (6::Int) "issue warning when type constructor depth exceeds this number"
defineFlag "typeConDepthCritical" (9::Int) "issue critical when type constructor depth exceeds this number"

assessTypeConDepth :: Assessment ConDepth
assessTypeConDepth (fromIntegral -> cy)
                 | cy > flags_typeConDepthWarning  = (Warning, "should be less than "        ++
                                                                show flags_typeConDepthWarning )
                 | cy > flags_typeConDepthCritical = (Warning, "must never be as high as " ++
                                                                show flags_typeConDepthCritical)
                 | otherwise                       = (Info,    ""                              )

defineFlag "numFunArgsWarning"  (5::Int) "issue warning when number of function arguments exceeds this number"
defineFlag "numFunArgsCritical" (9::Int) "issue critical when number of function arguments exceeds this number"

assessNumFunArgs :: Assessment NumFunArgs
assessNumFunArgs (fromIntegral -> cy)
                 | cy > flags_numFunArgsWarning  = (Warning, "should be less than " ++ show flags_numFunArgsWarning )
                 | cy > flags_numFunArgsCritical = (Warning, "must never reach "    ++ show flags_numFunArgsCritical)
                 | otherwise                     = (Info,    ""                                                     )

metrics :: [Program -> Log]
metrics  = [measureTopOccurs assessModuleLength   locT        moduleT
           ,measureTopOccurs assessFunctionLength locT        functionT
           ,measureTopOccurs assessFunctionDepth  depthT      functionT
           ,measureTopOccurs assessFunctionCC     cyclomaticT functionT
           ,measureTopOccurs assessTypeConDepth   conDepthT   typeSignatureT
           ,measureTopOccurs assessNumFunArgs     numFunArgsT typeSignatureT]
