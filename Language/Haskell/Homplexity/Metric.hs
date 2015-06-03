{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Class for defining code metrics, and its simplest implementation - number of lines of code. 
module Language.Haskell.Homplexity.Metric (
    Metric (..)
  , LOC
  , locT
  , measureAs
  , measureFor
  ) where

import Data.Data
import Data.Function
import Data.Functor
import Data.Generics.Uniplate.Data
import Data.List
import Control.Arrow
import Language.Haskell.Exts.Syntax

import Language.Haskell.Homplexity.CodeFragment

-- | Metric can be computed on a set of @CodeFragment@ fragments
-- and then shown.
class (CodeFragment c, Show m) => Metric m c where
  measure :: c -> m

-- | Number of lines of code
-- (example metric)
newtype LOC = LOC { asInt :: Int }
  deriving (Ord, Eq, Enum, Num, Real, Integral)

-- | Proxy for passing @LOC@ type as parameter.
locT :: Proxy LOC
locT  = Proxy

instance Show LOC where
  showsPrec _ (LOC l) = shows l . (" lines of code"++)

instance Read LOC where
  readsPrec prec str = first LOC <$> readsPrec prec str

instance (CodeFragment c) => Metric LOC c where
  measure = LOC
          . length                          -- total number of lines that contain at least one object with SrcLoc
          . concatMap (nub . map srcLine)   -- remove duplicate lines within the same file
          . groupBy ((==) `on` srcFilename) -- group by filename
          . universeBi                      -- all SrcLoc objects

-- | Convenience function for fixing the @Metric@ type.
measureAs :: (Metric m c) => Proxy m -> c -> m
measureAs _ = measure

-- | Convenience function for fixing both the @Metric@ and @CodeFragment@ for which the metric is computed.
measureFor :: (Metric m c) => Proxy m -> Proxy c -> c -> m
measureFor _ _ = measure
