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
  , Program      (..)
  , Function     (..)
  , TypeSignature(..)
  -- TODO: add ClassSignature
  ) where

import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Exception
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts

--import Language.Haskell.Homplexity.SrcSlice

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
  deriving (Data, Typeable)

-- TODO: class signatures (number of function decls inside)
-- ** Alias for a class signature
data ClassSignature = ClassSignature
  deriving (Data, Typeable)

{-

data Message = Message { msgSeverity :: Severity
                       , msgText     :: String
                       , msgSrc      :: SrcLoc   } -}

-- TODO: need combination of Fold and Biplate
-- Resulting record may be created to make pa

-- | Class @Code@ allows for:
-- * both selecting direct or all descendants
--   of the given type of object within another structure
--   (with @occurs@ and @allOccurs@)
-- * naming the object to allow user to distinguish it.
--
-- In order to compute selection, we just need to know which
-- @AST@ nodes contain the given object, and how to extract
-- this given object from @AST@, if it is there (@matchAST@).:w
class (Data (AST c), Data c) => Code c where
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

instance Code Program where
  type AST Program = Program
  matchAST  = Just
  name _    = "program"

instance Code Module where
  type AST Module = Module
  matchAST = Just 
  name (Module _ (ModuleName theName) _ _ _ _ _) = "module " ++ theName

instance Code TypeSignature where
  type AST TypeSignature = Decl
  matchAST (TypeSig loc identifiers theType) = Just $ TypeSignature {..}
  matchAST  _                                = Nothing
  name (TypeSignature {..}) = "type signature for " ++ intercalate ", " (map unName identifiers)

unName ::  Name -> String
unName (Symbol s) = s
unName (Ident  i) = i 

