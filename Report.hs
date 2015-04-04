{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Report (
    Reportable (..)
  , report
  , fatal
  ) where

import System.IO
import System.Exit

-- TODO: convert to Text builder
-- TODO: work with monad-journal?

rep1 :: a -> IO ()

{-
class Report a where
  report :: a -> IO ()

instance (Reportable a) => Report a where
  report = output

instance (Reportable a, Report b) => Report (a -> b) where
  report a b = do output a
                  report b
 -}

report = undefined

output :: (Reportable a) => a -> IO ()
output  = hPutStrLn stderr . reportAs

fatal re = do report re
              exitFailure

class Reportable a where
  reportAs :: a -> String
  --reportAs  = show

instance Reportable String where
  reportAs = id

reportListed :: (Reportable a) => [a] -> String
reportListed  = unwords . map reportAs

{-
instance (Reportable a, Reportable b) => Reportable (a, b) where
  reportAs (a, b) = reportListed [a, b]-}

instance {-# INCOHERENT #-} (Show a) => Reportable a where
  reportAs = show

