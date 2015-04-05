{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Showing references to slices of code
module Language.Haskell.Homplexity.Metric (
    SrcSlice
  , srcSlice
  ) where

import Data.Generics.Uniplate.Data
import Data.List
import Control.Arrow
import Control.Exception (assert)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts

-- * Slice of code
data SrcSlice = SrcSlice {
    sliceFilename  ::  String
  , sliceFirstLine
  , sliceLastLine  ::  Int
  -- TODO: do we want to show columns too?
  --, sliceLocs      :: [SrcLoc]
  }

-- | Compute the slice of code that given source fragment is in (for naming)
srcSlice code = assert (allEqual $ map srcFilename sliceLocs) $
    case sliceLocs of
      []    -> error $ "Don't know how make a SrcSlice for this code fragment:" ++ show code
      _     -> SrcSlice {..}
  where
    sliceFilename                   = srcFilename $ head sliceLocs
    (sliceFirstLine, sliceLastLine) = (minimum &&& maximum) $
                                      map srcLine sliceLocs
    sliceLocs                      :: [SrcLoc]
    sliceLocs                       = universeBi code

allEqual       ::  Eq a => [a] -> Bool
allEqual []     = True
allEqual (b:bs) = all (b==) bs

instance Show SrcSlice where
  showsPrec _ (SrcSlice {..}) = shows sliceFilename
                              . (':':)
                              . shows sliceFirstLine
                              . ('-':)
                              . shows sliceLastLine

