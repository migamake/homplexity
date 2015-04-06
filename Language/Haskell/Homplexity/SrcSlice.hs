{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Showing references to slices of code
module Language.Haskell.Homplexity.SrcSlice (
    SrcSlice
  , srcSlice
  , srcLoc
  , showSrcSpan
  , mergeSrcLocs
  , sliceFirstLine
  , sliceLastLine
  , sliceFilename
  ) where

import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Control.Arrow
import Control.Exception (assert)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc

-- * Slice of code
type SrcSlice  = SrcSpan
sliceFilename  = srcSpanFilename
sliceFirstLine = srcSpanStartLine
sliceLastLine  = srcSpanEndLine

srcLoc :: (Data code, Show code) => code -> SrcLoc
srcLoc code = checkHead  $
              universeBi   code
  where
    msg              = "Cannot find SrcLoc in the code fragment: " ++ show code
    checkHead []     = error msg
    checkHead (e:es) = e

-- | Compute the slice of code that given source fragment is in (for naming)
srcSlice code = mergeSrcLocs
              . checkNonEmpty
              . universeBi    $ code
  where
    checkNonEmpty []    = error $ "Can't know how make a SrcSlice from code fragment: " ++ show code
    checkNonEmpty other = other

mergeSrcLocs :: [SrcLoc] -> SrcSpan
mergeSrcLocs []        = error "Don't know how make a SrcSpan from an empty list of locations!"
mergeSrcLocs sliceLocs = assert (allEqual $ map srcFilename sliceLocs) $
                           SrcSpan {..}
  where
    srcSpanFilename = srcFilename $ head sliceLocs
    ((srcSpanStartLine, srcSpanStartColumn),
     (srcSpanEndLine,   srcSpanEndColumn  )) = (minimum &&& maximum) $
                                               map (srcLine &&& srcColumn) sliceLocs

allEqual       ::  Eq a => [a] -> Bool
allEqual []     = True
allEqual (b:bs) = all (b==) bs

showSrcSpan               :: SrcSpan -> ShowS
showSrcSpan (SrcSpan {..}) = shows srcSpanFilename
                           . (':':)
                           . shows srcSpanStartLine
                           . ('-':)
                           . shows srcSpanEndLine

