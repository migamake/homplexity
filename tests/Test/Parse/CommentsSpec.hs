{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
module Test.Parse.CommentsSpec where


import Test.Hspec

import Language.Haskell.Homplexity.Comments

import Test.Utils


spec :: Spec
spec = describe "tests for comments" $ do
    it "must parse comments after" $ example $
        findCommentType "  |" `shouldBe` CommentsAfter
    it "must parse comments before" $ example $
        findCommentType "  ^" `shouldBe` CommentsBefore
    it "must parse comments group" $ example $
        findCommentType "  *" `shouldBe` CommentsInside
    it "must parse comments inside" $ example $
        findCommentType "  a" `shouldBe` CommentsInside
    it "must output comments" $ example $ do
        (_ast, comments) <- [tsrc|
            module Amanitas where
            -- | This is comment preceeding variable "a"
            a=1
            b=2
            -- ^ This is comment following variable "b"
            |]
        (length comments) `shouldBe` 2
