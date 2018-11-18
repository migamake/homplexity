{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Computing cyclomatic complexity and branching depth.
module Language.Haskell.Homplexity.Cyclomatic(
    Cyclomatic
  , cyclomaticT
  , Depth
  , depthT) where

import Data.Data
import Data.Generics.Uniplate.Data
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Metric

type MatchSet = [Match SrcLoc]

-- * Cyclomatic complexity
-- | Represents cyclomatic complexity
newtype Cyclomatic = Cyclomatic { unCyclo :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | For passing @Cyclomatic@ type as parameter.
cyclomaticT :: Proxy Cyclomatic 
cyclomaticT  = Proxy

instance Show Cyclomatic where
  showsPrec _ (Cyclomatic cc) = ("cyclomatic complexity of " ++)
                              . shows cc

instance Metric Cyclomatic Function where
  measure x = Cyclomatic . cyclomatic $ x

-- | Computing cyclomatic complexity on a code fragment
cyclomatic :: Data from => from -> Int
cyclomatic x = cyclomaticOfMatches x
             + cyclomaticOfExprs   x
             + 1

-- | Sum the results of mapping the function over the list.
sumOf :: (a -> Int) -> [a] -> Int
sumOf f = sum . map f

-- | Compute cyclomatic complexity of pattern matches.
cyclomaticOfMatches :: Data from => from -> Int
cyclomaticOfMatches  = sumOf recurse . childrenBi
  where
    recurse   :: MatchSet -> Int
    recurse  x = length x - 1 +  sumOf cyclomaticOfMatches x

-- | Cyclomatic complexity of all expressions
cyclomaticOfExprs :: forall from.
        Data from => from -> Int
cyclomaticOfExprs = sumOf armCount . (universeBi :: from -> [Exp SrcLoc])
  where
    armCount (If      {}    ) = 2           - 1
    armCount (MultiIf _ alts) = length alts - 1
    armCount (LCase   _ alts) = length alts - 1
    armCount (Case  _ _ alts) = length alts - 1
    armCount _              = 0               -- others are ignored

-- * Decision depth
-- | Sum the results of mapping the function over the list.
maxOf :: (a -> Int) -> [a] -> Int
maxOf f = maximum . (0:). map f

-- | Decision depth
newtype Depth = Depth Int
  deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | For passing @Depth@ type as parameter.
depthT :: Proxy Depth 
depthT  = Proxy

instance Metric Depth Function where
  measure (Function {..}) = Depth $ depthOfMatches functionRhs `max` depthOfMatches functionBinds

instance Show Depth where
  showsPrec _ (Depth d) = ("branching depth of "++)
                        .  shows d

-- | Depth of branching within @Exp@ression.
depthOfExpr :: Exp SrcLoc -> Int
depthOfExpr x = fromEnum (isDecision x)+maxOf depthOfExpr (children x)

-- | Helper function to compute depth of branching within @case@ expression match.
depthOfMatches ::  Data from => [from] -> Int
depthOfMatches []   = 0 -- Should never happen
depthOfMatches [m ] =   maxOf depthOfExpr           (childrenBi m )
depthOfMatches  ms  = 1+maxOf depthOfExpr (concatMap childrenBi ms)

-- | Check whether given @Exp@ression node is a decision node (conditional branch.)
isDecision           :: Exp SrcLoc -> Bool
isDecision If      {} = True
isDecision MultiIf {} = True 
isDecision LCase   {} = True
isDecision Case    {} = True
isDecision _          = False

