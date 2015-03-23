{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
module Language.Haskell.Homplexity.Cyclomatic(cyclomatic) where

import Data.Data
import Data.Generics.Uniplate.Data
import Language.Haskell.Exts.Syntax

type MatchSet = [Match]

-- * Cyclomatic complexity
-- | Computing cyclomatic complexity on a code fragment
cyclomatic :: Data from => from -> Int
cyclomatic x = cyclomaticOfMatches x
             + cyclomaticOfExprs   x

-- | Sum the results of mapping the function over the list.
sumOf :: (a -> Int) -> [a] -> Int
sumOf f = sum . map f

-- | Compute cyclomatic complexity of pattern matches.
cyclomaticOfMatches :: Data from => from -> Int
cyclomaticOfMatches = sumOf recurse . childrenBi
  where
    recurse   :: MatchSet -> Int
    recurse  x = length x - 1 +  sumOf cyclomaticOfMatches x

-- | Cyclomatic complexity of all expressions
cyclomaticOfExprs :: Data from => from -> Int
cyclomaticOfExprs = sumOf armCount . universeBi
  where
    armCount (If      {}  ) = 2           - 1
    armCount (MultiIf alts) = length alts - 1
    armCount (LCase   alts) = length alts - 1
    armCount _              = 0               -- others are ignored

