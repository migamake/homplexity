{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
module Language.Haskell.Homplexity.Comments (
    CommentLink      (..)
  , CommentType      (..)
  , classifyComments
  , findCommentType -- ^ exposed for testing only
  ) where

import Data.Char
import Data.List
import Control.Exception as E

import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts

-- | Describes the comment span, and the way it may be connected to the
-- source code
data CommentLink = CommentLink { commentSpan :: SrcSpan
                               , commentType :: CommentType
                               }
  deriving(Eq, Ord, Show)

-- | Possible link between comment and commented entity.
data CommentType = CommentsBefore -- ^ May be counted as commenting object that starts just before.
                 | CommentsInside -- ^ May be counted as commenting object within which it exists.
                 | CommentsAfter  -- ^ May be counted as commenting object that starts just after.
  deriving (Eq, Ord, Enum, Show)

-- | Classifies all comments in list, so they can be assigned to declarations later.
classifyComments :: [Comment] -> [CommentLink]
classifyComments  = map classifyComment
  where
    classifyComment (Comment _ commentSpan (findCommentType -> commentType)) = CommentLink {..}

-- | Finds Haddock markers of which declarations the comment pertains to.
findCommentType :: String -> CommentType
findCommentType txt = case find (not . isSpace) txt of
  Just '^' -> CommentsBefore
  Just '|' -> CommentsAfter
  Just '*' -> CommentsInside -- since it comments out the group of declarations, it belongs to the containing object
  _        -> CommentsInside

-- * Tests for comments
prop_commentsAfter :: Bool
prop_commentsAfter  = findCommentType "  |" == CommentsAfter
prop_commentsBefore = findCommentType "  ^" == CommentsBefore
prop_commentsGroup  = findCommentType "  *" == CommentsInside
prop_commentsInside = findCommentType "  a" == CommentsInside

