{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables #-}
module Test.Utilities
  ( testFile
  , measureSumOnFile
  ) where

import System.FilePath (FilePath, (</>))

import Language.Haskell.Exts (Extension)
import Language.Haskell.Homplexity.CodeFragment (occurs)
import Language.Haskell.Homplexity.Metric (Metric(..))
import Language.Haskell.Homplexity.Parse (parseSource)

-- | Constructs OS-independent path to test file
testFile :: FilePath -> FilePath
testFile fn = "tests" </> "test-data" </> fn

measureSumOnFile :: forall m c. (Metric m c, Num m) => [Extension] -> FilePath -> IO m
measureSumOnFile exts path = do
  result <- parseSource exts path
  p <- case result of
    Left  logs      -> error $ show logs
    Right (prog, _) -> pure prog
  let codeFrags = occurs p :: [c]
  pure $ sum $ map measure codeFrags
