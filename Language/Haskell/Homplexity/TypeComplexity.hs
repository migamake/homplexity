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
import Data.Proxy(Proxy)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.Metric
import Debug.Trace

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

{-# INLINE maxConDepth #-}
maxConDepth = maxOf conDepth

-- | Function computing constructor depth of a @Type@.
conDepth :: Type -> Int
conDepth (TyVar _)                         = 0
conDepth (TyCon _)                         = 0
conDepth (TyForall _bind _context theType) = 1+conDepth theType
conDepth (TyList theType)                  = 1+conDepth theType
conDepth (TyParen theType)                 = conDepth theType 
conDepth (TyKind theType _kind)            = conDepth theType
conDepth (TyFun type1 type2)               = 1+maxConDepth [type1, type2]
conDepth (TyApp type1 type2)               = 1+maxConDepth [type1, type2]
conDepth (TyInfix type1 _ type2)           = 1+maxConDepth [type1, type2]
conDepth (TyTuple _boxed types)            = 1+maxConDepth types

-- * Number of function arguments
newtype NumFunArgs = NumFunArgs { unNumFunArgs :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

numFunArgsT :: Proxy NumFunArgs
numFunArgsT  = Proxy

instance Show NumFunArgs where
  showsPrec _ (NumFunArgs cc) =  shows cc
                              . (" arguments"    ++)

instance Metric NumFunArgs TypeSignature where
  measure = NumFunArgs . numFunArgs . theType

-- | Function computing constructor depth of a @Type@.
numFunArgs :: Type -> Int
numFunArgs (TyVar _)                         = 1
numFunArgs (TyCon _)                         = 1
numFunArgs (TyForall _bind _context theType) = 1+numFunArgs theType
numFunArgs (TyList theType)                  = 1
numFunArgs (TyParen theType)                 = numFunArgs theType
numFunArgs (TyKind theType _kind)            = numFunArgs theType
numFunArgs (TyFun type1 type2)               = 1+numFunArgs type2
numFunArgs (TyApp type1 type2)               = 1
numFunArgs (TyInfix type1 _ type2)           = 1
numFunArgs (TyTuple _boxed types)            = 1

