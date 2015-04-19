{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Main (
    main 
  ) where

import Data.Char
import Data.List
import Control.Exception as E

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts

import Language.Haskell.Homplexity.Comments

-- * Tests for comments
prop_commentsAfter ::  Bool
prop_commentsAfter  = findCommentType "  |" == CommentsAfter

prop_commentsBefore ::  Bool
prop_commentsBefore = findCommentType "  ^" == CommentsBefore

prop_commentsGroup ::  Bool
prop_commentsGroup  = findCommentType "  *" == CommentsInside

prop_commentsInside ::  Bool
prop_commentsInside = findCommentType "  a" == CommentsInside

-- Runs all unit tests.
main :: IO ()
main  = assert (and [prop_commentsAfter
                    ,prop_commentsBefore
                    ,prop_commentsGroup
                    ,prop_commentsInside]) $
          return ()
