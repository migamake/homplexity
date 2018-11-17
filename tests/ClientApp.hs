-- | Crash on missing module name
import qualified Client

import System.Environment (getArgs)

import PaxosTypes

main :: IO ()
main
-- Parse command line args
 = do
    args <- getArgs
    let nodeNum:port:cmd:_ = args
    let op =
            case cmd of
                "get" -> Get (args !! 3)
                "set" -> Set (args !! 3) (args !! 4)
                "delete" -> Delete (args !! 3)
                "ping" -> Ping
    -- Start client
    Client.client (read nodeNum) port op
