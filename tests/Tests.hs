{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where


import Test.Framework

import {-@ HTF_TESTS @-} Test.Parse.Comments
import {-@ HTF_TESTS @-} Test.Parse.Extensions


main :: IO ()
main = htfMain htf_importedTests
