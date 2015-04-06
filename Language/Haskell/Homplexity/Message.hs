{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Language.Haskell.Homplexity.Message (
    Log
  , Message
  , warn
  , info
  , debug
  , extract
  ) where

import Control.Arrow
import Data.Function (on)
import Data.Sequence as Seq
import Data.Foldable as Foldable
import Language.Haskell.Homplexity.CodeFragment
import Language.Haskell.Exts

-- | Keeps a set of messages
newtype Log = Log { unLog :: Seq Message }
  deriving(Monoid)

-- | Message from analysis
data Message = Message { msgSeverity :: Severity
                       , msgText     :: String
                       , msgSrc      :: SrcLoc
                       }
  deriving (Eq)

instance Show Message where
  showsPrec _ (Message {..}) = shows msgSeverity
                             . (':':)
                             . (msgText++)
                             . ('\n':)

-- | Message severity
data Severity = Debug
              | Info
              | Warning
              | Error
  deriving (Eq, Ord, Read, Enum, Bounded)

instance Show Severity where
  showsPrec _ Error   = ("ERROR"  ++)
  showsPrec _ Warning = ("WARNING"++)
  showsPrec _ Info    = ("INFO"   ++)
  showsPrec _ Debug   = ("DEBUG"  ++)

-- | Helper for logging a message with given severity.
message ::  Severity -> SrcLoc -> String -> Log
message msgSeverity msgSrc msgText = Log $ Seq.singleton $ Message {..}

-- | TODO: automatic inference of the srcLine 
-- | Log a certain error
error :: SrcLoc -> String -> Log
error  = message Error

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
orderedMessages                    :: Severity -> Log -> Seq Message
orderedMessages severity (Log {..}) = Seq.unstableSortBy         msgOrdering  $
                                      Seq.filter ((severity<=) . msgSeverity)   unLog

-- | Extract an ordered sequence of messages from the @Log@.
extract ::  Severity -> Log -> [Message]
extract severity = Foldable.toList
                 . orderedMessages severity

instance Show Log where
  showsPrec _ l e = Foldable.foldr  shows e $
                    orderedMessages Debug l

