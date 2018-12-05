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

  , orderCommentsAndCommentables
  ) where

import Data.Char
import Data.Data
import Data.Function
import Data.Functor
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.PQueue.Max as Prio

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
findCommentType txt = case (not . isSpace) `find` txt of
  Just '^' -> CommentsBefore
  Just '|' -> CommentsAfter
  Just '*' -> CommentsInside -- since it comments the group of declarations, it belongs to the containing object
  _        -> CommentsInside

-- * Finding ranges of all commentable entities.
-- | Tagging of source range for each commentable object.
data CommentSite = CommentSite { siteName  :: String
                               , siteSlice :: SrcSlice
                               }
  deriving (Eq, Show)

newtype Ends   = End   { siteEnded   :: CommentSite }
  deriving (Eq, Show)

compareStarts :: CommentSite -> CommentSite -> Ordering
compareStarts = compare `on` start . siteSlice

instance Ord Ends   where
  compare = compareEnds `on` siteEnded

compareEnds :: CommentSite -> CommentSite -> Ordering
compareEnds  = compare `on` end   . siteSlice

start, end :: SrcSlice -> (Int, Int)
start slice = (srcSpanStartColumn slice, srcSpanStartLine slice)
end   slice = (srcSpanEndColumn   slice, srcSpanEndLine   slice)

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

-- | Take together are commentable elements, and all comments, and order them by source location.
orderCommentsAndCommentables :: [CommentSite] -> [CommentLink] -> [Either CommentLink CommentSite]
orderCommentsAndCommentables sites comments  = sortBy (compare `on` loc) elts
  where
    loc :: Either CommentLink CommentSite -> (SrcSpan, Bool)
    loc (Left  (commentSpan -> srcSpan)) = (srcSpan, True )
    loc (Right (siteSlice   -> srcSpan)) = (srcSpan, False)
    elts = (Left <$> comments) ++ (Right <$> sites)

type Assignment = Map.Map CommentSite [CommentLink]
{-
-- | Assign comments to the commentable elements.
assignComments :: [Either CommentLink CommentSite]
               -> [Assignment]
assignComments  = foldr assign ([], [], [], [])
  where
    assign :: (Assignment, [Assignment], [CommentLink]
    assign (assigned, unclosed, commentingAfter) nextElt = case nextElt of
      Left  (s@(CommentSite {}))                            ->
        (assigned, (s,commentingAfter):unclosed, [])
      Right (c@(CommentLink {commentType=CommentAfter,  ..}) -> 
        (assigned,                     unclosed, c:commentingAfter)
      Right (c@(CommentLink {commentType=CommentBefore, ..}) -> 
        (assigned,                     unclosed, c:commentingAfter)
      Right (c@(CommentLink {commentType=CommentInside, ..}) -> 
        (assigned,                     unclosed, c:commentingAfter)
 -}
