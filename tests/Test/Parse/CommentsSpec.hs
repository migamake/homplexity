{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module Test.Parse.CommentsSpec where


import Test.Hspec

import Language.Haskell.Homplexity.Comments

spec :: Spec
spec = describe "tests for comments" $ do
    it "must parse comments after" $
        findCommentType "  |" `shouldBe` CommentsAfter
    it "must parse comments before" $
        findCommentType "  ^" `shouldBe` CommentsBefore
    it "must parse comments group" $
        findCommentType "  *" `shouldBe` CommentsInside
    it "must parse comments inside" $
        findCommentType "  a" `shouldBe` CommentsInside
