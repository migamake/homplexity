{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
module Language.Haskell.Homplexity.Cyclomatic(cyclomatic, depth) where

import Data.Data
import Data.Generics.Uniplate.Data
import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.Matcher

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

-- | Sum the results of mapping the function over the list.
maxOf :: (a -> Int) -> [a] -> Int
maxOf f = maximum . map f

-- * Decision depth
--dcond :: Data from => from -> Int
--dcond = maxOf depthOfMatches . childrenBi

depthOfMatches (FunBind [m ] ) =   maxOf depthOfExpr           (childrenBi m )
depthOfMatches (FunBind  ms  ) = 1+maxOf depthOfExpr (concatMap childrenBi ms)
depthOfMatches  _              = 0

depthOfExpr :: Exp -> Int
depthOfExpr x = fromEnum (isDecision x)+maxOf depthOfExpr (children x)

isDecision             :: Exp -> Bool
isDecision (If      {}) = True
isDecision (MultiIf {}) = True 
isDecision (LCase   {}) = True
isDecision _            = False

depth = undefined
