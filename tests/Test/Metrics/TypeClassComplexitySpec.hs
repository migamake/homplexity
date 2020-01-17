module Test.Metrics.TypeClassComplexitySpec where


import Test.Hspec
import Test.Utilities

import Language.Haskell.Exts.Extension
import Language.Haskell.Homplexity.TypeClassComplexity
import Language.Haskell.Homplexity.CodeFragment (TypeClass, occurs)
import Language.Haskell.Homplexity.Metric (measure)
import Language.Haskell.Homplexity.Parse (parseSource)

spec :: Spec
spec = describe "tests for type class complexity" $ do
  it "correctly counts methods and values" $ flip shouldReturn (5 :: NonTypeDeclCount) $ do
    result <- parseSource [] (testFile "TypeClassComplexityTest.hs")
    p <- case result of
      Left  logs      -> error $ show logs
      Right (prog, _) -> pure prog
    let tyClassDecls = occurs p :: [TypeClass]
    pure $ sum $ map measure tyClassDecls

  it "correctly counts associated types" $ flip shouldReturn (5 :: AssocTypeCount) $ do
    result <- parseSource [EnableExtension TypeFamilies] (testFile "TypeClassComplexityTest.hs")
    p <- case result of
      Left  logs      -> error $ show logs
      Right (prog, _) -> pure prog
    let tyClassDecls = occurs p :: [TypeClass]
    pure $ sum $ map measure tyClassDecls

