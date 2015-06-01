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
module Main (main) where

import Data.Data
import Data.List
import Control.Monad

import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Cyclomatic
import Language.Haskell.Homplexity.Message
import Language.Haskell.Homplexity.Metric
import Language.Haskell.Homplexity.Parse
import System.Directory
import System.FilePath
import System.IO

import HFlags

-- * Command line flags
defineFlag "severity" Info (concat ["level of output verbosity (", severityOptions, ")"])
defineFlag "fakeFlag" Info "this flag is fake"

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

assessFunctionLength :: Assessment LOC
assessFunctionLength lines | lines > 20 = (Warning,  "should be kept below 20 lines of code." )
                | lines > 40 = (Critical, "this function exceeds 50 lines of code.")
                | otherwise  = (Info,     ""                                       )

assessModuleLength :: Assessment LOC
assessModuleLength lines | lines > 500  = (Warning,  "should be kept below 500 lines of code."  )
                         | lines > 3000 = (Critical, "this function exceeds 3000 lines of code.")
                         | otherwise    = (Info,     ""                                         )

assessFunctionDepth :: Assessment Depth
assessFunctionDepth depth | depth > 4 = (Warning, "should have no more than four nested conditionals"    )
                          | depth > 8 = (Warning, "should never exceed 8 nesting levels for conditionals")
                          | otherwise = (Info,    ""                                                     )

assessFunctionCC :: Assessment Cyclomatic
assessFunctionCC cy | cy > 20   = (Warning, "should not exceed 20"  )
                    | cy > 50   = (Warning, "should never exceed 50")
                    | otherwise = (Info,    ""                      )

metrics :: [Program -> Log]
metrics  = [measureTopOccurs assessModuleLength   locT        moduleT ,
            measureTopOccurs assessFunctionLength locT        functionT,
            measureTopOccurs assessFunctionDepth  depthT      functionT,
            measureTopOccurs assessFunctionCC     cyclomaticT functionT]

-- | Report to standard error output.
report ::  String -> IO ()
report = hPutStrLn stderr

-- | Analyze single source module.
analyzeModule ::  Module -> IO ()
analyzeModule  = analyzeModules . (:[])

-- | Analyze a set of modules.
analyzeModules ::  [Module] -> IO ()
analyzeModules = putStr . concatMap show . extract flags_severity . mconcat metrics . Program

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

-- | Process each separate input file.
processFile ::  FilePath -> IO Bool
processFile filepath = do src <- parseSource filepath
                          case src of
                            Left  msg              -> do report $ show msg
                                                         return False
                            Right (ast, _comments) -> do analyzeModule ast
                                                         return True

-- | Commonly defined function - should be added to base...
concatMapM  :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

testFilename :: FilePath
testFilename = "Homplexity.hs"

main :: IO ()
main = do
  args <- $initHFlags "json-autotype -- automatic type and parser generation from JSON"
  if null args
    then    void $ processFile testFilename
    else do sums <- mapM processFile =<< concatMapM subTrees args
            putStrLn $ unwords ["Correctly parsed", show $ length $ filter id sums,
                                "out of",           show $ length             sums,
                                "input files."]

