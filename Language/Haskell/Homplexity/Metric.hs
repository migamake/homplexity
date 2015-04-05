{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Homplexity.Metric (
    Metric (..)
  , LOC
  , measureAs
  , measureFor
  ) where

import Data.Data
import Data.Function
import Data.Generics.Uniplate.Data
import Data.List
import Control.Arrow
import Control.Exception (assert)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts

import Language.Haskell.Homplexity.CodeFragment

-- | Metric can be computed on a set of @CodeFragment@ fragments
-- and then shown.
class (CodeFragment c, Show m) => Metric m c where
  measure :: c -> m

-- | Number of lines of code
-- (example metric)
newtype LOC = LOC { asInt :: Int }
  deriving (Ord, Eq, Enum, Num, Real, Integral)

instance Show LOC where
  showsPrec _ (LOC l) = shows l . shows "lines of code"

instance (CodeFragment c) => Metric LOC c where
  measure = LOC
          . length
          . nub
          . sort
          . map ((srcFilename . head) &&& map srcLine)
          . groupBy ((==) `on` srcFilename)
          . universeBi

-- | Convenience function for fixing the @Metric@ type.
measureAs :: (Metric m c) => Proxy m -> c -> m
measureAs _ = measure

-- | Convenience function for fixing both the @Metric@ and @CodeFragment@ for which the metric is computed.
measureFor :: (Metric m c) => Proxy m -> Proxy c -> c -> m
measureFor _ _ = measure
