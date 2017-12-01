module Main where

import           Client
import           Client.Types
import           ClientStates.Lobby
import           ClientStates.Waiting
import           Game
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

server :: N.PortID -> IO ()
server port = do
  -- Apparently required to get instant output with systemd
  hSetBuffering stdout LineBuffering

  socket <- N.listenOn $ port
  putStrLn $ "Listening on port " ++ show port ++ ", waiting for players"

  waitingQueue <- atomically $ newTQueue
  lobbyQueue <- atomically $ newTQueue
  serverState <- atomically $ newTVar $ MkServerState { games = [], inQueue = Nothing }

  C.forkIO $ waitingLoop waitingQueue lobbyQueue serverState
  C.forkIO $ lobbyLoop waitingQueue lobbyQueue serverState
  acceptConnections waitingQueue socket 0

acceptConnections :: ClientQueue Client -> NS.Socket -> Int -> IO ()
acceptConnections queue socket nextId = do
  (clientSocket, _) <- NS.accept socket
  handle <- NS.socketToHandle clientSocket ReadWriteMode
  hSetBuffering handle LineBuffering
  C.forkIO $ handleClient queue handle nextId
  acceptConnections queue socket $ nextId + 1
