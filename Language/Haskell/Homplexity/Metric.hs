{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Homplexity.Metric (
    Metric (..)
  , LOC
  ) where

import Data.Data
import Data.Function
import Data.Generics.Uniplate.Data
import Data.List
import Control.Arrow
import Control.Exception (assert)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts

import Language.Haskell.Homplexity.Code

-- | Metric can be computed on a set of @Code@ fragments
-- and then shown.
class (Code c, Show m) => Metric m c where
  measure :: c -> m

-- | Number of lines of code
-- (example metric)
newtype LOC = LOC { asInt :: Int }
  deriving (Ord, Eq, Enum, Num, Real, Integral)

instance Show LOC where
  showsPrec _ (LOC l) = shows l . shows "lines of code"

instance (Code c) => Metric LOC c where
  measure = LOC
          . length
          . nub
          . sort
          . map ((srcFilename . head) &&& map srcLine)
          . groupBy ((==) `on` srcFilename)
          . universeBi

