{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Data
import Distribution.PackageDescription.Parsec
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Language.Haskell.Extension
import Text.Pretty.Simple
import qualified Data.ByteString as BS

import Data.Generics.Uniplate.Data as U


pp :: (Show a) => a -> IO ()
pp = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg


data X = A [Extension]
       | B Extension
       | C Int
       | D X X X
       deriving (Show, Data, Typeable)


main = do
    let struct = D (B (EnableExtension OverlappingInstances))
                   (A [EnableExtension DoRec, DisableExtension RecursiveDo, UnknownExtension "ZZZ"])
                   (C 42)
        extensions = [x | EnableExtension x <- U.universe struct]
    pp struct
    pp extensions


    {-
    cabalFile <- BS.readFile "homplexity.cabal"
    let (_, Right res) = runParseResult $ parseGenericPackageDescription cabalFile
        (Just lib) = condLibrary res
        libData = condTreeData lib
        buildInfo = libBuildInfo libData
        exts = otherExtensions buildInfo
        exts' = [e | EnableExtension e <- U.universe lib]
        -- exts' = [e | EnableExtension e <- U.universe buildInfo]
        -- exts' = (U.universe buildInfo) :: [Extension]
    pp exts
    putStrLn "************************"
    pp exts'
    -}
