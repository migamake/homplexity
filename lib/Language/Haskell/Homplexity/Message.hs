{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
-- | Classifying messages by severity and filtering them.
module Language.Haskell.Homplexity.Message (
    Log
  , Message
  , Severity (..)
  , severityOptions
  , critical
  , warn
  , info
  , debug
  , message
  , extract
  ) where

import Control.Arrow
import Control.DeepSeq
import Data.Function                                        (on)
import Data.Foldable                            as Foldable
#if __GLASGOW_HASKELL__ >= 800
import Data.Semigroup(Semigroup(..))
#else
import Data.Monoid
#endif
import Data.Sequence                            as Seq
import Language.Haskell.Exts hiding (style)
import Language.Haskell.TH.Syntax                           (Lift(..))
import HFlags
#ifdef HTML_OUTPUT
import Prelude hiding (head, id, div, span)
import Text.Blaze.Html4.Strict hiding (map, style)
import Text.Blaze.Html4.Strict.Attributes hiding (title, span)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
#endif

-- | Keeps a set of messages
newtype Log = Log { unLog :: Seq Message }
  deriving(Monoid
#if __GLASGOW_HASKELL__ >= 800
          ,Semigroup
#endif
                    )

instance NFData Log where
  rnf = rnf . unLog

-- | Message from analysis
data Message = Message { msgSeverity :: !Severity
                       , msgText     :: !String
                       , msgSrc      :: !SrcLoc
                       }
  deriving (Eq)

instance NFData Message where
  rnf Message {..} = rnf msgSeverity `seq` rnf msgText `seq` rnf msgSrc

instance NFData SrcLoc where
  rnf SrcLoc {..} = rnf srcFilename `seq` rnf srcLine `seq` rnf srcColumn

instance Show Message where
  showsPrec _ Message {msgSrc=loc@SrcLoc{..}, ..} = shows msgSeverity
                                                  . (':':)
                                                  . (srcFilename++)
                                                  . (':':)
                                                  . shows loc
                                                  -- . shows srcLine
                                                  -- . shows srcColumn
                                                  . (": "++)
                                                  . (msgText++)
                                                  . ('\n':)

#ifdef HTML_OUTPUT
instance ToMarkup Message where
  toMarkup Message {..} =
    p ! classId $
     (toMarkup msgSeverity
       <> string ": "
       <> toMarkup msgSrc
       <> string ": "
       <> string msgText)
    where
      classId = case msgSeverity of
                     Debug    -> class_ "debug"
                     Info     -> class_ "info"
                     Warning  -> class_ "warning"
                     Critical -> class_ "critical"

instance ToMarkup Severity where
  toMarkup Debug    = span   ! class_ "severity" $ string (show Debug)
  toMarkup Info     = span   ! class_ "severity" $ string (show Info)
  toMarkup Warning  = strong ! class_ "severity" $ string (show Warning)
  toMarkup Critical = strong ! class_ "severity" $ string (show Critical)

instance ToMarkup SrcLoc where
  toMarkup SrcLoc {..} = a ! href (toValue srcFilename) $ (string srcFilename)

#endif

-- | Message severity
data Severity = Debug
              | Info
              | Warning
              | Critical
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

instance NFData Severity where
  rnf !_a = ()

-- | String showing all possible values for @Severity@.
severityOptions :: String
severityOptions  = unwords $ map show [minBound..(maxBound::Severity)]

instance Lift Severity where
  lift Debug    = [| Debug    |]
  lift Info     = [| Info     |]
  lift Warning  = [| Warning  |]
  lift Critical = [| Critical |]

instance FlagType Severity where
  defineFlag n v = defineEQFlag n [| v :: Severity |] "{Debug|Info|Warning|Critical}"

-- | Helper for logging a message with given severity.
message ::  Severity -> SrcLoc -> String -> Log
message msgSeverity msgSrc msgText = Log $ Seq.singleton Message {..}

-- | TODO: automatic inference of the srcLine
-- | Log a certain error
critical :: SrcLoc -> String -> Log
critical  = message Critical

-- | Log a warning
warn  ::  SrcLoc -> String -> Log
warn   = message Warning

-- | Log informational message
info  ::  SrcLoc -> String -> Log
info   = message Info

-- | Log debugging message
debug ::  SrcLoc -> String -> Log
debug  = message Debug

-- TODO: check if this is not too slow
msgOrdering ::  Message -> Message -> Ordering
msgOrdering = compare `on` ((srcFilename &&& srcLine) . msgSrc)

-- | Convert @Log@ into ordered sequence (@Seq@).
orderedMessages                  :: Severity -> Log -> Seq Message
orderedMessages severity Log {..} = Seq.unstableSortBy         msgOrdering  $
                                      Seq.filter ((severity<=) . msgSeverity)   unLog

-- | Extract an ordered sequence of messages from the @Log@.
extract ::  Severity -> Log -> [Message]
extract severity = Foldable.toList
                 . orderedMessages severity

instance Show Log where
  showsPrec _ l e = Foldable.foldr  shows e $
                    orderedMessages Debug l

