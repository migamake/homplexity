{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module allows embedding source modules
-- Example:
-- 
-- example1 :: IO ParseResult
-- example1 = [tsrc|
-- mod ule Main where
-- a=1
-- |]
-- -- Filename and line where error happens will be correctly reported by Haskell parser.

module TestSource(tsrc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Language.Haskell.Homplexity.Parse(parseTest)

-- | QuasiQuoter for a non-interpolating String
tsrc :: QuasiQuoter
tsrc  = QuasiQuoter  embedSource
                    (error "Cannot use tsrc as a pattern")
                    (error "Cannot use tsrc as a type"   )
                    (error "Cannot use tsrc as a dec"    )
embedSource :: String -> Q Exp
embedSource aString = do
  loc <- location
  let theString = linePragma loc ++ aString      -- ^ Passes the information about correct location to the source
  let testNote  = "Test line "   ++ showLine loc -- ^ Shown in case that LINE pragma is not parsed
  [|parseTest testNote|] `appE` [| theString |]
-- parseTest :: FilePath -> String -> IO ParseResult

linePragma    :: Loc -> String
linePragma loc = concat ["{-# LINE ", showLine loc, " \"", loc_filename loc, "\" #-}\n"]

showLine :: Loc -> String
showLine = show . fst . loc_start
