{-# OPTIONS_GHC -F -pgmF htfpp     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Test.Parse.Comments (htf_thisModulesTests) where


import Test.Framework

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
