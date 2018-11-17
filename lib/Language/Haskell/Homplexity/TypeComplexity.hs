{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Computing cyclomatic complexity and branching depth.
module Language.Haskell.Homplexity.TypeComplexity(
    ConDepth
  , conDepthT
  , NumFunArgs
  , numFunArgsT) where

import Data.Data
import Data.Generics.Uniplate.Data
--import Data.Proxy(Proxy)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Metric
--import Debug.Trace

-- | Sum the results of mapping the function over the list.
maxOf :: (a -> Int) -> [a] -> Int
maxOf f = maximum . (0:). map f

-- * Depth of type constructor nesting
newtype ConDepth = ConDepth { unConDepth :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

conDepthT :: Proxy ConDepth
conDepthT  = Proxy

instance Show ConDepth where
  showsPrec _ (ConDepth cc) = ("type constructor nesting of " ++)
                            . shows cc

instance Metric ConDepth TypeSignature where
  measure = ConDepth . conDepth . theType

-- | Function computing constructor depth of a @Type@.
conDepth :: (Eq a, Data a) => Type a -> Int
conDepth con = deeper con + maxOf conDepth (filter (/= con) $ childrenBi con)

-- | Check whether given constructor of @Type@ counts in constructor depth computation.
deeper :: Type a -> Int
deeper (TyForall   _ _bind _context _type) = 1
deeper (TyList     _ _aType         )      = 1
deeper (TyFun      _ _type1   _type2)      = 1
deeper (TyApp      _ _type1   _type2)      = 1
deeper (TyInfix    _ _type1 _ _type2)      = 1
deeper (TyTuple    _ _boxed   _types)      = 1
deeper (TyParArray _          _types)      = 1
deeper  _                                  = 0

-- * Number of function arguments
newtype NumFunArgs = NumFunArgs { _unNumFunArgs :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

numFunArgsT :: Proxy NumFunArgs
numFunArgsT  = Proxy

instance Show NumFunArgs where
  showsPrec _ (NumFunArgs cc) =  shows cc
                              . (" arguments"    ++)

instance Metric NumFunArgs TypeSignature where
  measure = NumFunArgs . numFunArgs . theType

-- | Function computing constructor depth of a @Type@.
numFunArgs :: Type a -> Int
numFunArgs (TyParen    _ aType)                 =   numFunArgs aType
numFunArgs (TyKind     _ aType  _kind)          =   numFunArgs aType
numFunArgs (TyForall   _ _bind  _context aType) =   numFunArgs aType -- NOTE: doesn't count type argument
numFunArgs (TyFun      _ _type1 type2)          = 1+numFunArgs type2
numFunArgs (TyParArray _ aType)                 = 1+numFunArgs aType
numFunArgs  _                                   = 1

