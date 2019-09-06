{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Test.Parse.Extensions (htf_thisModulesTests) where


import System.FilePath
import Data.Either
import Language.Haskell.Exts.Extension

import Test.Framework

import Language.Haskell.Homplexity.Parse


-- | Constructs OS-independent path to test file
testFile :: FilePath -> FilePath
testFile fn = "tests" </> "test-data" </> fn


test_canParseLanguagePragmaInFile :: IO ()
test_canParseLanguagePragmaInFile =
    parseSource [] (testFile "test0001.hs") >>= assertBool . isRight


test_can'tParseWithoutLanguagePragmasInFile :: IO ()
test_can'tParseWithoutLanguagePragmasInFile =
    parseSource [] (testFile "test0002.hs") >>= assertBool . isLeft


test_canParseWithAdditionalPragmas :: IO ()
test_canParseWithAdditionalPragmas =
    parseSource [EnableExtension ScopedTypeVariables] (testFile "test0002.hs") >>= assertBool . isRight


test_pragmasInFileOverridesAdditionalPragmas :: IO ()
test_pragmasInFileOverridesAdditionalPragmas =
    parseSource [DisableExtension ScopedTypeVariables] (testFile "test0001.hs") >>= assertBool . isRight


test_disablingWorksAsDisabling :: IO ()
test_disablingWorksAsDisabling =
    parseSource [DisableExtension ScopedTypeVariables] (testFile "test0002.hs") >>= assertBool . isLeft
