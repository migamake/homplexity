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

measureAll  :: (CodeFragment c, Metric m c) => Severity -> (Program -> [c]) -> Proxy m -> Proxy c -> Program -> Log
measureAll severity generator metricType fragType = mconcat
                                                  . map       (showMeasure severity metricType fragType)
                                                  . generator

measureTopOccurs  :: (CodeFragment c, Metric m c) => Severity -> Proxy m -> Proxy c -> Program -> Log
measureTopOccurs severity = measureAll severity occurs

measureAllOccurs  :: (CodeFragment c, Metric m c) => Severity -> Proxy m -> Proxy c -> Program -> Log
measureAllOccurs severity = measureAll severity allOccurs

showMeasure :: (CodeFragment c, Metric m c) => Severity -> Proxy m -> Proxy c -> c -> Log
showMeasure severity metricType fragType c = message severity (        fragmentLoc  c)
                                                              (concat [fragmentName c
                                                                      ," has "
                                                                      ,show result   ])
  where
    result = measureFor metricType fragType c

metrics :: [Program -> Log]
metrics  = [measureTopOccurs Info  locT        programT,
            measureTopOccurs Debug locT        functionT,
            measureTopOccurs Debug depthT      functionT,
            measureTopOccurs Debug cyclomaticT functionT]

report = hPutStrLn stderr

analyzeModule  = analyzeModules . (:[])

analyzeModules = print . mconcat metrics . Program

-- | Find all Haskell source files within a given path.
-- Recurse down the tree, if the path points to directory.
subTrees          :: FilePath -> IO [FilePath]
-- Recurse to . or .. only at the first level, to prevent looping:
subTrees dir      | dir `elem` [".", ".."] = concatMapM subTrees' =<< getDirectoryPaths dir
subTrees filepath                          = do
  isDir <- doesDirectoryExist filepath
  if isDir
     then subTrees' filepath
     else do
       exists <- doesFileExist filepath
       if exists
          then    return [filepath]
          else do report $ "File does not exist: " ++ filepath
                  return []

-- | Return filepath if normal file, or recurse down the directory if it is not special directory ("." or "..")
subTrees'                       :: FilePath -> IO [FilePath]
subTrees' (takeFileName -> "..") = return []
subTrees' (takeFileName -> "." ) = return []
subTrees'  fp                    = do
  isDir <- doesDirectoryExist fp
  if isDir
    then concatMapM subTrees' =<< getDirectoryPaths fp
    else return $ filter (".hs" `isSuffixOf`) [fp]

-- | Get contents of a given directory, and return their full paths.
getDirectoryPaths        :: FilePath -> IO [FilePath]
getDirectoryPaths dirPath = map (dirPath </>) <$> getDirectoryContents dirPath

processFile :: FilePath -> IO ()
processFile filename = do
  parsed <- parseFile filename
  case parsed of
    ParseOk parsed -> analyzeModule parsed
    other          -> report $ show other

-- | Commonly defined function - should be added to base...
concatMapM  :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

defineFlag "v:verbosity" Info (concat ["level of output verbosity (", severityOptions, ")"])

main :: IO ()
main = do
  args <- $initHFlags "json-autotype -- automatic type and parser generation from JSON"
  if null args
    then processFile "Test.hs"
    else mapM_ processFile =<< debugM =<< concatMapM subTrees args

debugM list = do print $ unwords list
                 return          list

