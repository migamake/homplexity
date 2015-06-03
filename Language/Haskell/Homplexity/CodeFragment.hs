{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
-- | This module generalizes over types of code fragments
-- that may need to be iterated upon and measured separately.
module Language.Haskell.Homplexity.CodeFragment (
    CodeFragment(fragmentName)
  , occurs
  , allOccurs
  , Program      (..)
  , programT
  , Module       (..)
  , moduleT
  , Function     (..)
  , functionT
  , TypeSignature(..)
  , typeSignatureT
  , fragmentLoc
  -- TODO: add ClassSignature
  ) where

import Data.Data
import Data.Generics.Uniplate.Data
import Data.List
import Data.Maybe
import Control.Arrow
import Control.Exception
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts
import Language.Haskell.Homplexity.SrcSlice

-- | Program
data Program = Program { allModules :: [Module] }
  deriving (Data, Typeable, Show)

-- | Smart constructor for adding cross-references in the future.
program  = Program

-- | Proxy for passing @Program@ type as an argument.
programT :: Proxy Program
programT  = Proxy

-- * Type aliases for type-based matching of substructures
-- | Alias for a function declaration
data Function = Function {
                  functionNames     :: [String]
                , functionLocations :: [SrcLoc]
                , functionRhs       :: [Rhs]
                , functionBinds     :: [Binds]
                }
  deriving (Data, Typeable, Show)

-- | Proxy for passing @Function@ type as an argument.
functionT :: Proxy Function
functionT  = Proxy

-- ** Type signature of a function
-- | Type alias for a type signature of a function as a @CodeFragment@
data TypeSignature = TypeSignature { loc         :: SrcLoc
                                   , identifiers :: [Name]
                                   , theType     :: Type }
  deriving (Data, Typeable, Show)

-- | Proxy for passing @Program@ type as an argument.
typeSignatureT :: Proxy TypeSignature
typeSignatureT  = Proxy

-- ** TODO: class signatures (number of function decls inside)
-- | Alias for a class signature
data ClassSignature = ClassSignature
  deriving (Data, Typeable)

-- TODO: need combination of Fold and Biplate
-- Resulting record may be created to make pa

-- | Class @CodeFragment@ allows for:
-- * both selecting direct or all descendants
--   of the given type of object within another structure
--   (with @occurs@ and @allOccurs@)
-- * naming the object to allow user to distinguish it.
--
-- In order to compute selection, we just need to know which
-- @AST@ nodes contain the given object, and how to extract
-- this given object from @AST@, if it is there (@matchAST@).:w
class (Show c, Data (AST c), Data c) => CodeFragment c where
  type             AST c
  matchAST      :: AST c -> Maybe c
  fragmentName  ::     c -> String
  fragmentSlice ::     c -> SrcSlice
  fragmentSlice  = srcSlice

fragmentLoc :: (CodeFragment c) => c -> SrcLoc
fragmentLoc =  getPointLoc
            .  fragmentSlice

instance CodeFragment Function where
  type AST Function     = Decl
  matchAST (FunBind matches) = Just Function {..}
    where
      (functionLocations, (unName <$>) . take 1 -> functionNames, functionRhs, functionBinds) = unzip4 $ map extract matches
      extract (Match srcLoc name _ _ rhs binds) = (srcLoc, name, rhs, binds)
  matchAST (PatBind (singleton -> functionLocations) pat
                    (singleton -> functionRhs      )
                    (singleton -> functionBinds    )) = Just Function {..}
    where
      functionNames  = wildcards ++ map unName (universeBi pat)
      wildcards = mapMaybe wildcard $ universeBi pat
        where
          wildcard PWildCard = Just    ".."
          wildcard _         = Nothing
  matchAST _                                          = Nothing
  fragmentName (Function {..}) = unwords $ "function":functionNames

-- | Make a single element list.
singleton :: a -> [a]
singleton  = (:[])

-- | Direct occurences of given @CodeFragment@ fragment within another structure.
occurs :: (CodeFragment c, Data from) => from -> [c]
occurs  = mapMaybe matchAST . childrenBi

-- | All occurences of given type of @CodeFragment@ fragment within another structure.
allOccurs :: (CodeFragment c, Data from) => from -> [c]
allOccurs = mapMaybe matchAST . universeBi

instance CodeFragment Program where
  type AST Program = Program
  matchAST         = Just
  fragmentName _   = "program"

instance CodeFragment Module where
  type AST Module = Module
  matchAST = Just 
  fragmentName (Module _ (ModuleName theName) _ _ _ _ _) = "module " ++ theName

-- | Proxy for passing @Module@ type as an argument.
moduleT :: Proxy Module
moduleT  = Proxy

instance CodeFragment TypeSignature where
  type AST TypeSignature = Decl
  matchAST (TypeSig loc identifiers theType) = Just TypeSignature {..}
  matchAST  _                                = Nothing
  fragmentName (TypeSignature {..}) = "type signature for " ++ intercalate ", " (map unName identifiers)

-- | Unpack @Name@ identifier into a @String@.
unName ::  Name -> String
unName (Symbol s) = s
unName (Ident  i) = i 

