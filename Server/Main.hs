module Main where


import           CardGame.Client
import           CardGame.Types
import qualified Control.Concurrent     as C
import           Control.Concurrent.STM
import           Data.Maybe             (fromMaybe, listToMaybe)
import qualified Network                as N
import qualified Network.Socket         as NS
import           System.Environment     (getArgs)
import           System.IO              (BufferMode (LineBuffering),
                                         IOMode (ReadWriteMode), hSetBuffering,
                                         stdout)
import           Text.Read              (readMaybe)

defaultPort :: NS.PortNumber
defaultPort = 8081

-- Gets the port number as either the first CLI argument or the default
getPort :: IO N.PortID
getPort = do
  args <- getArgs
  return $ N.PortNumber $ fromMaybe defaultPort $ listToMaybe args >>= readMaybe

main :: IO ()
main = do
  -- Apparently required to get instant output with systemd
  hSetBuffering stdout LineBuffering

  port <- getPort
  socket <- N.listenOn $ port
  putStrLn $ "Listening on port " ++ show port ++ ", waiting for players"

  serverState <- atomically $ newTVar $ MkServerState { games = [], inQueue = Nothing }
  acceptConnections serverState socket 0



acceptConnections :: TVar ServerState -> NS.Socket -> Int -> IO ()
acceptConnections serverState socket nextId = do
  (clientSocket, _) <- NS.accept socket
  handle <- NS.socketToHandle clientSocket ReadWriteMode
  hSetBuffering handle LineBuffering
  _ <- C.forkIO $ clientHandler serverState handle nextId
  acceptConnections serverState socket $ nextId + 1


