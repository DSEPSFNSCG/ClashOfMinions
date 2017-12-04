module Main where

import           Client
import           Client.Types
import           Game
import           Server
import           Types


import qualified Control.Concurrent              as C
import           Control.Concurrent.STM
import           Control.Monad.STM.Class
import           Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import           Data.Maybe                      (fromMaybe, listToMaybe)
import qualified Network                         as N
import qualified Network.Socket                  as NS
import           System.Environment              (getArgs)
import           System.IO                       (BufferMode (LineBuffering),
                                                  IOMode (ReadWriteMode),
                                                  hSetBuffering, stdout)
import           Text.Read                       (readMaybe)

defaultPort :: NS.PortNumber
defaultPort = 8081


-- Gets the port number as either the first CLI argument or the default
getPort :: IO N.PortID
getPort = do
  args <- getArgs
  return $ N.PortNumber $ fromMaybe defaultPort $ listToMaybe args >>= readMaybe

main :: IO ()
main = do
  port <- getPort
  server port
