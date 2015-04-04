{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
-- | This module generalizes over types of code fragments
-- that may need to be iterated upon and measured separately.
module Language.Haskell.Homplexity.Code (
    Code(name)
  , occurs
  , allOccurs
  , Program
  , Function
  , TypeSignature
  -- TODO: add ClassSignature
  , SrcSlice
  , srcLines
  , numLoc
  ) where

import Data.Data
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Exception

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts

import Data.Generics.Uniplate.Data

-- | Program
data Program = Program { allModules :: [Module] }
  deriving (Data, Typeable, Show)

-- | Smart constructor for adding cross-references in the future.
program  = Program

-- * Type aliases for type-based matching of substructures
-- | Alias for a function declaration
data Function = Function { functionMatches :: [Match] }
  deriving (Data, Typeable, Show)

-- ** Alias for a type signature of a function
data TypeSignature = TypeSignature { loc         :: SrcLoc
                                   , identifiers :: [Name]
                                   , theType     :: Type }
  deriving (Data, Typeable, Show)

-- TODO: class signatures (number of function decls inside)
-- ** Alias for a class signature
data ClassSignature = ClassSignature
  deriving (Data, Typeable)

{-
data Metric codeFrag unit = Metric {
    Show (codeFrag, unitOfMetric)
  , codeFrag     :: *
  , unitOfMetric :: *
  , name         :: String
  , severity     :: Severity
  , compute      :: codeFrag -> metricUnit
  }

data Message = Message { msgSeverity :: Severity
                       , msgText     :: String
                       , msgSrc      :: SrcLoc   }

data Program = Program [Module]

-- NamedCode a == Biplate Program codeFragment

makeMetric :: (Biplate Program codeFragment, Show unit) =>
                 MetricCalculator codeFragment unit ->
                 MetricChecker    codeFragment unit

-- * Ops on metrics:
data ExecutableMetric = {
    Metric codeFrag unit :: *
  -- | Compute metric for each relevant fragment.
  , computeMetric :: (Biplate codeFrag1 codeFrag2           ) => Metric codeFrag2 unit  -> codeFrag1 -> unit
  -- | Show metric for each relevant fragment.
  , showMetric    :: (Biplate codeFrag1 codeFrag2, Show unit) => Metric codeFrag2 unit  -> codeFrag1 -> [Message]
  -- | Check computed metric for each relevant fragment.
  , checkMetric   :: (Biplate codeFrag1 codeFrag2           ) => Metric codeFrag2 alpha -> codeFrag1 -> [Message]
  }

-- TODO: need combination of Fold and Biplate
-- Resulting record may be created to make pa

class NamedCode c =>

class (NamedCode c) => CheckMetric a b where


 -}

-- | Check that all elements of a given list are equal.
allEqual ::  Eq a => [a] -> Bool
allEqual []     = True
allEqual (b:bs) = all (b==) bs

-- | Number of effective (non-comment, non-empty) source lines withi a given code fragment.
srcLines ::  Data from => from -> Int
srcLines frag = check $ if null allLines
                          then 0
                          else length $ nub $ sort allLines
  where
    check    = assert $ allEqual $ map srcFilename locs
    allLines = map srcLine locs
    locs    :: [SrcLoc]
    locs     = universeBi frag

data SrcSlice = SrcSlice {
    sliceFilename  ::  String
  , sliceFirstLine
  , sliceLastLine  ::  Int
  -- TODO: do we want to show columns too?
  , sliceLocs      :: [SrcLoc]
  }

instance Show SrcSlice where
  show (SrcSlice {..}) = unwords [sliceFilename, show sliceFirstLine ++ "-" ++ show sliceLastLine]

-- | Class @Code@ allows for:
-- * both selecting direct or all descendants
--   of the given type of object within another structure
--   (with @occurs@ and @allOccurs@)
-- * naming the object to allow user to distinguish it.
--
-- In order to compute selection, we just need to know which
-- @AST@ nodes contain the given object, and how to extract
-- this given object from @AST@, if it is there (@matchAST@).:w
class (Data (AST c)) => Code c where
  type        AST c
  matchAST :: AST c -> Maybe c
  name     ::     c -> String

instance Code Function where
  type AST Function = Decl
  matchAST (FunBind ms) = Just $ Function ms
  matchAST  _           = Nothing
  name     (Function (Match _ theName _ _ _ _:_)) = "function " ++ unName theName

-- | Direct occurences of given @Code@ fragment within another structure.
occurs :: (Code c, Data from) => from -> [c]
occurs  = mapMaybe matchAST . childrenBi

-- | All occurences of given type of @Code@ fragment within another structure.
allOccurs :: (Code c, Data from) => from -> [c]
allOccurs = mapMaybe matchAST . universeBi

{-
-- | Code fragment
class Code c where
  occurs         :: (Data prog) => prog -> [c]
  allOccurs      :: (Data prog) => prog -> [c]
 -}

instance Code Program where
  type AST Program = Program
  matchAST  = Just
  name _    = "program"

instance Code Module where
  type AST Module = Module
  matchAST = Just 
  name (Module _ (ModuleName theName) _ _ _ _ _) = "module " ++ theName

{-
instance Named Decl where
  name = show . srcSlice-}

{-
instance {-# OVERLAPPED #-} (Biplate Module c, Data c) => Code c where
  occurs = universeBi . allModules
  name a = "fragment " ++ show (srcSlice a)
 -}

instance Code TypeSignature where
  type AST TypeSignature = Decl
  matchAST (TypeSig loc identifiers theType) = Just $ TypeSignature {..}
  matchAST  _                                = Nothing
  name (TypeSignature {..}) = "type signature for " ++ intercalate ", " (map unName identifiers)

unName ::  Name -> String
unName (Symbol s) = s
unName (Ident  i) = i 

--srcSlice :: NamedCode a => a -> SrcSlice
srcSlice codeCodement = assert (allEqual $ map srcFilename sliceLocs) $
                          case sliceLocs of
                            []    -> error $ "Don't know how make a SrcSlice for this code fragment:" ++ show codeCodement
                            _     -> SrcSlice {..}
  where
    sliceFilename                   = srcFilename $ head sliceLocs
    (sliceFirstLine, sliceLastLine) = (minimum &&& maximum) $
                                      map srcLine sliceLocs
    sliceLocs                      :: [SrcLoc]
    sliceLocs                       = universeBi codeCodement

-- Number of lines of code within @SrcSlice@
numLoc ::  SrcSlice -> [Int]
numLoc = nub
       . sort
       . map srcLine
       . sliceLocs
