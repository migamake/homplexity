{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main (main) where

import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Control.Arrow
import Control.Exception
import Control.Monad

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts
import Language.Haskell.Homplexity.Cyclomatic
import Language.Haskell.Homplexity.Metric
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Message
import System.Directory
import System.Environment
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

type Message = ShowS

--measureAll  :: (CodeFragment c, Metric m c) => (Program -> [c]) -> Proxy m -> Proxy c -> Program -> Log
measureAll severity generator metricType fragType = mconcat
                                                  . map       (showMeasure severity metricType fragType)
                                                  . generator

--measureTopOccurs  :: (CodeFragment c, Metric m c) => Proxy m -> Proxy c -> Program -> Log
measureTopOccurs severity = measureAll severity occurs

--measureAllOccurs  :: (CodeFragment c, Metric m c) => Proxy m -> Proxy c -> Program -> Log
measureAllOccurs severity = measureAll severity allOccurs

--showMeasure :: (CodeFragment c, Metric m c) => Proxy m -> Proxy c -> c -> Log
showMeasure severity metricType fragType c = message severity (        fragmentLoc  c)
                                                              (concat [fragmentName c
                                                                      ," has "
                                                                      ,show result   ])
  where
    result = measureFor metricType fragType c

--          $
                             --map ($ prog) tests
--  where
--    a `merge` b = a . ('\n':) $ b

metrics :: [Program -> Log]
metrics  = [measureTopOccurs Info  locT        programT,
            measureTopOccurs Debug locT        functionT,
            measureTopOccurs Debug depthT      functionT,
            measureTopOccurs Debug cyclomaticT functionT]

report = hPutStrLn stderr

processFiles :: [FilePath] -> IO ()
processFiles filenames = do
  log <- mconcat metrics <$> parseFiles filenames
  print log

subTrees     :: FilePath -> IO [FilePath]
subTrees ".." = return []
subTrees  fp  = do
  isDir <- doesDirectoryExist fp
  if isDir
    then concatMapM subTrees =<< getDirectoryPaths fp
    else return [fp]

-- | Get contents of a given directory, and return their full paths.
getDirectoryPaths        :: FilePath -> IO [FilePath]
getDirectoryPaths dirPath = map addPrefix <$> (filterM isSourceFileOrDirectory =<< getDirectoryContents dirPath)
  where
    isSourceFileOrDirectory fp | ".hs" `isSuffixOf` fp = return True
                               | otherwise             = doesDirectoryExist fp
    addPrefix | dirPath == "." =  id
              | otherwise      = (dirPath </>)

parseFiles :: [FilePath] -> IO Program
parseFiles files = (Program . catMaybes) <$> mapM parseAndReportError files
  where
    parseAndReportError filename = do
      parsed <- parseFile filename
      case parsed of
        ParseOk r -> do --print r
                        return $ Just r
        other     -> do report $ show other
                        return   Nothing

-- | Commonly defined function - should be added to base...
concatMapM  :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

defineFlag "v:verbosity" Info (concat ["level of output verbosity (", unwords $ map show [minBound..(maxBound::Severity)], ")"])

main :: IO ()
main = do
  args <- $initHFlags "json-autotype -- automatic type and parser generation from JSON"
  if null args
    then processFiles ["Test.hs"]
    else processFiles =<< concatMapM subTrees args

