{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
module Homplexity where

import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Control.Arrow
import Control.Exception

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts
import Language.Haskell.Homplexity.Cyclomatic
import Language.Haskell.Homplexity.Metric
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Message
import System.Environment

data Severity = Error
              | Warn
              | Info
              | Debug
{-
numFunctions = length
             . filter isFunBind
             . getModuleDecls

testNumFunctions = (>20)

numFunctionsMsg = "More than 20 functions per module"

numFunctionsSeverity = Warning
 -}

type Message = ShowS

measureAll  :: (CodeFragment c, Metric m c) => (Program -> [c]) -> Proxy m -> Proxy c -> Program -> Log
measureAll generator metricType fragType = mconcat
                                         . map       (showMeasure metricType fragType)
                                         . generator

measureTopOccurs  :: (CodeFragment c, Metric m c) => Proxy m -> Proxy c -> Program -> Log
measureTopOccurs = measureAll occurs

measureAllOccurs  :: (CodeFragment c, Metric m c) => Proxy m -> Proxy c -> Program -> Log
measureAllOccurs = measureAll allOccurs

showMeasure :: (CodeFragment c, Metric m c) => Proxy m -> Proxy c -> c -> Log
showMeasure metricType fragType c = info (        fragmentLoc  c)
                                         (concat [fragmentName c
                                                 ," has "
                                                 ,show result   ])
  where
    result = measureFor metricType fragType c

--          $
                             --map ($ prog) tests
--  where
--    a `merge` b = a . ('\n':) $ b

_u :: [Program -> Log]
_u  = [measureTopOccurs locT        programT,
       measureTopOccurs locT        functionT,
       measureTopOccurs depthT      functionT,
       measureTopOccurs cyclomaticT functionT]

processFile         :: FilePath -> IO ()
processFile filename = do
  parsed <- parseFile "Test.hs"
  case parsed of
    ParseOk r -> do print r {-
      print $ funBinds r
      putStr  "All locations: "
      print $ (universeBi :: Module -> [SrcLoc]) r
      print   $ unlines $ map ( show . (fragmentName &&& ((' ':) . show . cyclomatic))) $ funBinds r-}
    other     -> print other

main :: IO ()
main = do
  args <- getArgs
  if null args
    then processFile "Test.hs"
    else mapM_ processFile args
