-- | Just testing some homplexity stuff...
--
module Main (main) where


import Language.Haskell.Homplexity.Parse
import Language.Haskell.Exts
import Text.Pretty.Simple


pp :: (Show a) => a -> IO ()
pp = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg


main :: IO ()
main = do
    let testFn = "tests/test-data/test0001.hs"
    src <- parseSource [EnableExtension ScopedTypeVariables] testFn
    pp src

