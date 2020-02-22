{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE CPP                   #-}
-- | Main module parsing inputs, and running analysis.
module Main (main) where

import Control.Monad(when)
import Data.Either
import Data.Functor
import Data.List hiding (head)
import Data.Monoid
import Data.Version

import GitHash(giHash, giDirty, giCommitDate, tGitInfoCwdTry)
import HFlags
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.Assessment
import Language.Haskell.Homplexity.CabalFiles
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Message
import Language.Haskell.Homplexity.Parse
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Paths_homplexity(version)

import qualified Prelude as P
import Prelude hiding (head, id, div)
#ifdef HTML_OUTPUT
import qualified Data.ByteString.Lazy as B
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html4.Strict hiding (map)
import Text.Blaze.Html4.Strict.Attributes hiding (title, style)
#endif

-- * Command line flag
defineFlag "v:version" False "Show version"

defineFlag "severity" Warning (concat ["level of output verbosity (", severityOptions, ")"])

data OutputFormat =
    Text
#ifdef HTML_OUTPUT
  | HTML
#endif
  deriving (Read, Show)
defineEQFlag "format" [| Text :: OutputFormat |]
#ifdef HTML_OUTPUT
  "{Text|HTML}"
#else
  "{Text}"
#endif
  "What format to use"

-- | Report to standard error output.
report :: String -> IO ()
report  = hPutStrLn stderr

-- * Recursing directory tree in order to list Haskell source files.
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

-- | Commonly defined function - should be added to base...
concatMapM  :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- * Analysis
-- | Analyze a set of modules.
analyzeModule :: Module SrcLoc -> IO ()
analyzeModule  = out
               . extract flags_severity
               . mconcat metrics
               . program
               . (:[])
  where
    out msgs = case flags_format of
                    Text -> do
                      putStr . concatMap show $ msgs
#ifdef HTML_OUTPUT
                    HTML -> B.putStr $ renderHtml $ do
                      html $ do
                        head $ do
                          title (string "Homplexity Analysis.")
                        style $ string style_head
                        body $ do
                          toHtml . fmap toHtml $ msgs
    style_head = ".warning  .severity { color: orange; }" <>
                 ".debug    .severity { color: grey;   }" <>
                 ".critical .severity { color: red;    }"
#endif

-- | Process each separate input file.
processFile :: [Extension] -> FilePath -> IO Bool
processFile additionalExtensions filepath =
    parseSource additionalExtensions filepath >>=
        either (\msg              -> report (show msg) >> return False)
               (\(ast, _comments) -> analyzeModule ast >> return True)


-- | This flag exists only to make sure that HFLags work.
defineFlag "cabal" "" "Project cabal file"

-- | This flag exists only to make sure that HFLags work.
defineFlag "fakeFlag" Info "this flag is fake"


versionString = showVersion version <> gitString
  where
    gitString = case $$tGitInfoCwdTry of
                  Left  err     -> "unknown revision: " <> err
                  Right gitInfo -> unwords [
                     "git rev",  giHash       gitInfo
                    ,"dated",    giCommitDate gitInfo
                    ,if          giDirty      gitInfo
                        then     "dirty"
                        else     ""]

-- | Parse arguments and either process inputs (if available), or suggest proper usage.
main :: IO ()
main = do
  args <- $initHFlags ("Homplexity " ++ versionString ++ " - automatic analysis of Haskell code quality")
  when flags_version $
    report $ unwords ["Version: ", versionString]

  if null args
    then do report ("Use Haskell source file or directory as an argument, " ++
                    "or use --help to discover options.")
            exitFailure
    else do exts <- cabalExtensions
            sums <- mapM (processFile exts) =<< concatMapM subTrees args
            report $ unwords ["Correctly parsed", show $ length $ filter P.id sums,
                              "out of",           show $ length               sums,
                              "input files."]

cabalExtensions :: IO [Extension]
cabalExtensions
  | null flags_cabal = return []
  | otherwise =
      parseCabalFile flags_cabal >>=
        either (\msg -> report (show msg) >> return [])
               (\cabal -> return $ languageExtensions Library cabal)


