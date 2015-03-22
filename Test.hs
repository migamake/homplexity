{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Homplexity where

import Data.Data
import Data.List
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

main :: IO ()
main = do
  ParseOk r <- parseFile "Test.hs"
  return (universeBi :: Module -> [Decl])
  print $ funBinds r 

