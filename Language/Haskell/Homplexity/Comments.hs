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
  , findCommentType -- exposed for testing only
  , CommentSite      (..)
  , commentable
  ) where

import Data.Char
import Data.Data
import Data.List
--import Control.Exception as E

import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Homplexity.SrcSlice
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
prop_commentsAfter, prop_commentsBefore, prop_commentsGroup, prop_commentsInside :: Bool
prop_commentsAfter  = findCommentType "  |" == CommentsAfter
prop_commentsBefore = findCommentType "  ^" == CommentsBefore
prop_commentsGroup  = findCommentType "  *" == CommentsInside
prop_commentsInside = findCommentType "  a" == CommentsInside

-- * Finding ranges of all commentable entities.
-- | Tagging of source range for each commentable object.
data CommentSite = CommentSite { siteName  :: String
                               , siteSlice :: SrcSlice
                               }

-- | Find comment sites for entire program.
commentable     :: Data from => from -> [CommentSite]
commentable code = ($ code) `concatMap` [slicesOf functionT
                                        ,slicesOf typeSignatureT
                                        ,slicesOf moduleT       ]
  where
    commentSite  ::  CodeFragment c => (c -> SrcSlice) -> c -> CommentSite
    commentSite with frag = CommentSite (fragmentName frag)
                                        (with         frag)
    commentSites :: (CodeFragment c, Data from) => (c -> SrcSlice) -> Proxy c -> from -> [CommentSite]
    commentSites with fragType = map (commentSite with) . occursOf fragType
    slicesOf :: (CodeFragment c, Data from) => Proxy c -> from -> [CommentSite]
    slicesOf = commentSites              fragmentSlice 
    --locsOf   = commentSites (locAsSpan . fragmentLoc)

