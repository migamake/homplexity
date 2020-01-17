module Test.Utilities
  ( testFile
  ) where

import System.FilePath

-- | Constructs OS-independent path to test file
testFile :: FilePath -> FilePath
testFile fn = "tests" </> "test-data" </> fn
