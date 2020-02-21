module Language.Haskell.Homplexity.Utilities(
    sumOf
  , maxOf
  , declHeadName
  ) where

import Language.Haskell.Exts.Syntax (DeclHead(..), Name)

-- | Maximum of the results of mapping the function over the list.
maxOf :: (a -> Int) -> [a] -> Int
maxOf f = maximum . (0:). map f

-- | Sum the results of mapping the function over the list.
sumOf :: (a -> Int) -> [a] -> Int
sumOf f = sum . map f

-- | Get the name of a declaration.
declHeadName :: DeclHead l -> Name l
declHeadName (DHead _ name)     = name
declHeadName (DHInfix _ _ name) = name
declHeadName (DHParen _ dh)     = declHeadName dh
declHeadName (DHApp _ dh _)     = declHeadName dh
