{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- | Measuring the complexity of type class declarations
module Language.Haskell.Homplexity.TypeClassComplexity
  ( NonTypeDeclCount
  , nonTypeDeclCountT
  , AssocTypeCount
  , assocTypeCountT
  ) where

import           Data.Data
import           Data.Generics.Uniplate.Data              ()
import           Data.Maybe
--import Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Homplexity.CodeFragment
import           Language.Haskell.Homplexity.Metric
import           Language.Haskell.Homplexity.Utilities

-- NOTE: It is non-trivial to decide whether a class declaration
-- is a method or a value. To correctly do that, we would have to
-- see through type synonyms. So, we will aggregate methods and values.

-- * Type class method count
-- | Represents the number of methods and value in a type class.
newtype NonTypeDeclCount = NonTypeDeclCount { unNonTypeDeclCount :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | For passing @NonTypeDeclCount@ type as parameter.
nonTypeDeclCountT :: Proxy NonTypeDeclCount
nonTypeDeclCountT  = Proxy

instance Show NonTypeDeclCount where
  showsPrec _ (NonTypeDeclCount mc) = ("method + value count of " ++)
                                    . shows mc

instance Metric NonTypeDeclCount TypeClass where
  measure = NonTypeDeclCount . sumOf method . fromMaybe [] . tcDecls where

    method :: ClassDecl l -> Int
    method (ClsDecl _ TypeSig{}) = 1
    method _                     = 0

-- * Type class associated types count
-- | Represents the number of associated types in a type class.
-- It includes both associated type and data families.
newtype AssocTypeCount = AssocTypeCount { unAssocTypeCount :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

-- | For passing @AssocTypeCount@ type as parameter.
assocTypeCountT :: Proxy AssocTypeCount
assocTypeCountT  = Proxy

instance Show AssocTypeCount where
  showsPrec _ (AssocTypeCount atc) = ("associated type count of " ++)
                                   . shows atc

instance Metric AssocTypeCount TypeClass where
  measure = AssocTypeCount . sumOf assocType . fromMaybe [] . tcDecls where

    assocType :: ClassDecl l -> Int
    assocType ClsTyFam{}   = 1
    assocType ClsTyDef{}   = 1
    assocType ClsDataFam{} = 1
    assocType _            = 0
