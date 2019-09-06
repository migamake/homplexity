{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
-- | Main module parsing inputs, and running analysis.
module Main (main) where


import Data.Functor
import Data.List
import Data.Monoid

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.Assessment
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Message
import Language.Haskell.Homplexity.Parse
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict as Map

import Language.Haskell.Exts

import Text.Pretty.Simple
import Data.Maybe


pp :: (Show a) => a -> IO ()
pp = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg


main :: IO ()
main = do
    let testFn = "test-data/test0001.hs"
        {-
    file <- readFile testFn
    let exts = readExtensions file
    let exts' = maybe [] snd exts
    pp exts
    pp exts'
    putStrLn "*************"
    pp (collapseSameExtensions exts')
    pp (collapseSameExtensions [ DisableExtension ScopedTypeVariables, EnableExtension ScopedTypeVariables, EnableExtension DoRec ])
    -}
    src <- parseSource [EnableExtension ScopedTypeVariables] testFn
    pp src

