{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module allows embedding source modules
module TestSource(tsrc) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified Language.Haskell.Exts.SrcLoc as Exts
import qualified Language.Haskell.Exts        as Exts

import Language.Haskell.Homplexity.Comments
import Language.Haskell.Homplexity.Parse(parseTest)
import Language.Haskell.Homplexity.Metric

-- | QuasiQuoter for a non-interpolating String
tsrc :: QuasiQuoter
tsrc  = QuasiQuoter  embeddedSource
                    (error "Cannot use q as a pattern")
                    (error "Cannot use q as a type"   )
                    (error "Cannot use q as a dec"    )

embeddedSource :: String -> Q Exp
embeddedSource aString = do
  loc <- location
  let theString = linePragma loc ++ aString
  let testNote  = "Test line "   ++ showLine loc
  [|parseTest testNote|] `appE` [| theString |]

linePragma    :: Loc -> String
linePragma loc = concat ["{-# LINE ", showLine loc, " \"", loc_filename loc, "\" #-}\n"]

showLine :: Loc -> String
showLine = show . fst . loc_start


