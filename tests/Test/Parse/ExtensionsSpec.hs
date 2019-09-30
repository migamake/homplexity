module Test.Parse.ExtensionsSpec where


import System.FilePath
import Language.Haskell.Exts.Extension

import Test.Hspec

import Language.Haskell.Homplexity.Parse
import Language.Haskell.Homplexity.CabalFiles


-- | Constructs OS-independent path to test file
testFile :: FilePath -> FilePath
testFile fn = "tests" </> "test-data" </> fn


spec :: Spec
spec = describe "extensions support" $ do
    it "must parse language pragma in file" $
        parseSource [] (testFile "test0001.hs") >>= (`shouldSatisfy` isRight)
    it "can parse language pragmas in file" $
        parseSource [] (testFile "test0001.hs") >>= (`shouldSatisfy` isRight)
    it "can't parse without language pragmas in file" $
        parseSource [] (testFile "test0002.hs") >>= (`shouldSatisfy` isLeft)
    it "can parse with additional pragmas" $
        parseSource [EnableExtension ScopedTypeVariables] (testFile "test0002.hs") >>= (`shouldSatisfy` isRight)
    it "pragmas in file must override additional pragmas" $
        parseSource [DisableExtension ScopedTypeVariables] (testFile "test0001.hs") >>= (`shouldSatisfy` isRight)
    it "disabling must works as disabling" $
        parseSource [DisableExtension ScopedTypeVariables] (testFile "test0002.hs") >>= (`shouldSatisfy` isLeft)
    -- * Testing for cabal file processing
    it "must extract language extensions from library from cabal" $
        parseCabalFile (testFile "test0003.cabal")
        >>= return . languageExtensions Library . fromRight (error "Can't parse test cabal file")
        >>= (`shouldBe` [EnableExtension FlexibleContexts,
                         EnableExtension FlexibleInstances,
                         EnableExtension UndecidableInstances,
                         EnableExtension OverlappingInstances])
    it "extract language extensions from package from cabal" $
        parseCabalFile (testFile "test0003.cabal")
        >>= return . languageExtensions (Package "test01") . fromRight (error "Can't parse test cabal file")
        >>= (`shouldBe` [EnableExtension DeriveDataTypeable,
                         EnableExtension RecordWildCards])


-- * Code from Data.Either --
-- Not supported by earlier 'base' package

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False


fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight def _ = def


-- fromLeft :: a -> Either a b -> a
-- fromLeft _ (Left a) = a
-- fromLeft def _ = def

