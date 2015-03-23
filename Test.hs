{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
module Homplexity where

import Data.Data
import Data.List
import Control.Arrow
import Control.Exception

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts
--import Data.Iterable
--import Data.Iterable.Instantiate

import Data.Generics.Uniplate.Data

getModuleDecls ::  Module -> [Decl]
getModuleDecls (Module _ _ _ _ _ _ decls) = decls

getAllDecls ::  Module -> [Decl]
getAllDecls = universeBi

isFunBind ::  Decl -> Bool
isFunBind (FunBind _) = True
isFunBind          _  = False

funBinds ::  Module -> [Decl]
funBinds = filter isFunBind
         . getModuleDecls

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

-- CodeFragment a == Biplate Program codeFragment

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

class CodeFragment c =>

class (CodeFragment c) => CheckMetric a b where


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

data Severity = Error
              | Warning
              | Informational

numFunctions = length
             . filter isFunBind
             . getModuleDecls

testNumFunctions = (>20)

numFunctionsMsg = "More than 20 functions per module"

numFunctionsSeverity = Warning

data SrcSlice = SrcSlice {
    sliceFilename  ::  String
  , sliceFirstLine
  , sliceLastLine  ::  Int
  -- TODO: do we want to show columns too?
  , sliceLocs      :: [SrcLoc]
  }

class (Show a, Data a, Biplate a SrcLoc) => CodeFragment a where
  -- | name of the object described by code fragment
  fragmentName :: a -> String
  isIgnored    :: a -> Bool

instance CodeFragment Module where
  fragmentName (Module _ (ModuleName theName) _ _ _ _ _) = "module " ++ theName
  isIgnored     _                                        = False

instance CodeFragment Decl where
  fragmentName (FunBind (Match _ theName _ _ _ _:_)) = "function " ++ unName theName
  fragmentName  _                                    = error "Not yet implemented!"
  isIgnored    (FunBind _                          ) = False
  isIgnored     _                                    = True

unName ::  Name -> String
unName (Symbol s) = s
unName (Ident  i) = i 

srcSlice :: CodeFragment a => a -> SrcSlice
srcSlice codeFragment = assert (allEqual $ map srcFilename sliceLocs) $
                          case sliceLocs of
                            []    -> error $ "Don't know how make a SrcSlice for this code fragment:" ++ show codeFragment
                            _     -> SrcSlice {..}
  where
    sliceFilename                   = srcFilename $ head sliceLocs
    (sliceFirstLine, sliceLastLine) = (minimum &&& maximum) $
                                      map srcLine sliceLocs
    sliceLocs                      :: [SrcLoc]
    sliceLocs                       = universeBi codeFragment

-- Number of lines of code within @SrcSlice@
numLoc ::  SrcSlice -> [Int]
numLoc = nub
       . sort
       . map srcLine
       . sliceLocs

headIfPresent ::  b -> [b] -> b
headIfPresent = foldr const

main :: IO ()
main = do
  parsed <- parseFile "Test.hs"
  case parsed of
    ParseOk r -> do 
      --return (universeBi :: Module -> [Decl])
      print $ funBinds r
      putStr "All locations: "
      print $ (universeBi :: Module -> [SrcLoc]) r
    other     -> print other
