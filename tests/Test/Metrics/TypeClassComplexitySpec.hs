{-# LANGUAGE TypeApplications #-}
module Test.Metrics.TypeClassComplexitySpec where


import Test.Hspec
import Test.Utilities

import Language.Haskell.Exts.Extension
import Language.Haskell.Homplexity.TypeClassComplexity
import Language.Haskell.Homplexity.CodeFragment (TypeClass)

spec :: Spec
spec = describe "tests for type class complexity" $ do
  it "correctly counts methods and values" $ flip shouldReturn (5 :: NonTypeDeclCount) $ do
    measureSumOnFile @NonTypeDeclCount @TypeClass
      [] (testFile "TypeClassComplexityTest.hs")

  it "correctly counts associated types" $ flip shouldReturn (5 :: AssocTypeCount) $ do
    measureSumOnFile @AssocTypeCount @TypeClass
      [EnableExtension TypeFamilies] (testFile "TypeClassComplexityTest.hs")


