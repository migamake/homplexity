{-# LANGUAGE TypeFamilies, DefaultSignatures #-}
module TypeClassComplexityTest where

class C a where
  type T1             -- counts as type
  type T2             -- counts as type
  type T3             -- counts as type

  type T4 = Int       -- counts as type

  data T5             -- counts as type

  f1 :: a -> Int      -- counts as non-type decl
  f2 :: a -> Int      -- counts as non-type decl
  f3 :: a -> Int      -- counts as non-type decl

  f4 :: a -> Int      -- counts as non-type decl
  f4 _ = 0

  x :: Maybe (a -> a) -- counts as non-type decl
  x = Nothing


