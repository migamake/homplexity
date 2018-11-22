{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
module Main (
    main 
  ) where

import Data.Char
import Data.List
import Control.Exception as E

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts

import Language.Haskell.Homplexity.Comments
import Language.Haskell.Homplexity.Parse
import Language.Haskell.Homplexity.Metric

import TestSource

-- * Tests for comment types
prop_commentsAfter ::  Bool
prop_commentsAfter  = findCommentType "  |" == CommentsAfter

prop_commentsBefore ::  Bool
prop_commentsBefore = findCommentType "  ^" == CommentsBefore

prop_commentsGroup ::  Bool
prop_commentsGroup  = findCommentType "  *" == CommentsInside

prop_commentsInside ::  Bool
prop_commentsInside = findCommentType "  a" == CommentsInside

testSrc = do
  (ast, comments) <- [tsrc|
module Amanitas where
-- | This is comment preceeding variable "a"
a=1
b=2
-- ^ This is comment following variable "b"
|]
  putStrLn $ "Comments:\n" ++ show comments

--src = $withLocation "mystring"

-- Runs all unit tests.
main :: IO ()
main  = do
  assert (and [prop_commentsAfter
              ,prop_commentsBefore
              ,prop_commentsGroup
              ,prop_commentsInside]) $
    return ()
  testSrc
