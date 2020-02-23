{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
module Language.Haskell.Homplexity.RecordFieldsCount(
    RecordFieldsCount
  , recordFieldsCountT
  ) where

--import Data.Maybe
import           Data.Proxy
import           Language.Haskell.Exts.SrcLoc
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Homplexity.CodeFragment
import           Language.Haskell.Homplexity.Metric
import           Language.Haskell.Homplexity.Utilities

newtype RecordFieldsCount = RecordFieldsCount { unFieldCount :: Int }
  deriving (Eq, Ord, Enum, Num, Real, Integral)

recordFieldsCountT :: Proxy RecordFieldsCount
recordFieldsCountT  = Proxy

instance Metric RecordFieldsCount DataDef where
  measure DataDef { .. } = RecordFieldsCount $ either measureCons measureGadts dataDefCtors

measureCons :: [QualConDecl SrcLoc] -> Int
measureCons = sumOf (\(QualConDecl _ _ _ decl) -> count decl)
  where
    count (ConDecl _ _ lst) = length lst
    count (RecDecl _ _ lst) = length lst
    count InfixConDecl {}   = 2

measureGadts :: [GadtDecl SrcLoc] -> Int
measureGadts = sumOf count
  where
#if MIN_VERSION_haskell_src_exts(1,21,0)
    count (GadtDecl _ _ _ _ maybeFields _) = maybe 0 length maybeFields
#else
    count (GadtDecl _ _     maybeFields _) = maybe 0 length maybeFields
#endif

instance Show RecordFieldsCount where
  showsPrec _ (RecordFieldsCount rfc) = ("record fields count of " ++)
                                      . shows rfc
