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

import Data.Functor
import Data.List
import Data.Monoid

import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.Assessment
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Message
import Language.Haskell.Homplexity.Parse
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import HFlags

-- * Command line flags
defineFlag "severity" Warning (concat ["level of output verbosity (", severityOptions, ")"])

-- | Report to standard error output.
report ::  String -> IO ()
report = hPutStrLn stderr

-- | Analyze single source module.
analyzeModule ::  Module -> IO ()
analyzeModule  = analyzeModules . (:[])

-- | Analyze a set of modules.
analyzeModules ::  [Module] -> IO ()
analyzeModules = putStr . concatMap show . extract flags_severity . mconcat metrics . program

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
concatMapM  :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | This flag exists only to make sure that HFLags work.
defineFlag "fakeFlag" Info "this flag is fake"

-- | Parse arguments and either process inputs (if available), or suggest proper usage.
main :: IO ()
main = do
  args <- $initHFlags "json-autotype -- automatic type and parser generation from JSON"
  if null args
    then do report ("Use Haskell source file or directory as an argument, " ++
                    "or use --help to discover options.")
            exitFailure
    else do sums <- mapM processFile =<< concatMapM subTrees args
            putStrLn $ unwords ["Correctly parsed", show $ length $ filter id sums,
                                "out of",           show $ length             sums,
                                "input files."]

