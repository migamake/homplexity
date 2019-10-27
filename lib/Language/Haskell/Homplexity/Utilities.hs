module Language.Haskell.Homplexity.Utilities(
    sumOf
  , maxOf
  ) where

-- | Maximum of the results of mapping the function over the list.
maxOf :: (a -> Int) -> [a] -> Int
maxOf f = maximum . (0:). map f

-- | Sum the results of mapping the function over the list.
sumOf :: (a -> Int) -> [a] -> Int
sumOf f = sum . map f
