module Main where


import           CardGame.Game
import           CardGame.Types

import qualified Control.Concurrent as C
import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Network            as N
import qualified Network.Socket     as NS
import           System.Environment (getArgs)
import           System.IO          (BufferMode (LineBuffering),
                                     IOMode (ReadWriteMode), hSetBuffering,
                                     stdout)
import           Text.Read          (readMaybe)


data ServerState = MkServerState { serverSocket  :: NS.Socket
                                 , games         :: [Game]
                                 , clientChannel :: ClientChannel
                                 , connections   :: ServerConnectionState
                                 , nextClientId  :: Int
                                 }

acceptConnection :: Int -> ClientChannel -> NS.Socket -> IO ()
acceptConnection nextClientId chan serverSocket = do
  (clientSocket, address) <- NS.accept serverSocket
  handle <- NS.socketToHandle clientSocket ReadWriteMode
  hSetBuffering handle LineBuffering

  let client = MkClient { clientId = nextClientId
                        , clientHandle = handle
                        , clientAddress = address }
  putStrLn $ "New client connected: " ++ show client
  C.writeChan chan (client, Connect)

acceptNext :: ServerState -> IO ServerState
acceptNext state@(MkServerState { nextClientId = nextClientId }) = do
  _ <- C.forkIO $ acceptConnection nextClientId (clientChannel state) (serverSocket state)
  return $ state { nextClientId = nextClientId + 1 }

data ServerConnectionState = ZeroConnections
                           | OneConnection Client
                           deriving (Show)

serverTrans :: ServerState -> (Client, ClientMessage) -> IO ServerState
serverTrans state (client, Connect) = do
  _ <- C.forkIO $ readClient client (clientChannel state)
  newState <- case connections state of
    ZeroConnections -> do
      return $ state { connections = OneConnection client
                     , nextClientId = (nextClientId state) + 1 }
    OneConnection client' -> do
      let gameId = length (games state)
      game <- newGame gameId client client' (clientChannel state)
      freshChannel <- C.newChan
      return $ state { connections = ZeroConnections
                     , clientChannel = freshChannel
                     , games = game:(games state)
                     , nextClientId = (nextClientId state) + 1 }
  acceptNext newState -- accept another connection
serverTrans state@(MkServerState { connections = ZeroConnections }) (_, Disconnect) = do
  putStrLn "[SERVERSTATE] Possible bug: Somebody not in the queue disconnected!"
  return state
serverTrans state@(MkServerState { connections = OneConnection client1 }) (client2, Disconnect) =
  if (client1 == client2)
  then do
    putStrLn "[SERVERSTATE] Somebody disconnected, queue empty"
    return $ state { connections = ZeroConnections }
  else do
    putStrLn "[SERVERSTATE] Possible bug: Somebody not in the queue disconnected!"
    return state
serverTrans state (client, RequestT _) = do
  clientSend client $ ErrorReply { errorMessage = "Game not started yet" }
  return state

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
  serverSocket <- N.listenOn $ port
  putStrLn $ "Listening on port " ++ show port ++ ", waiting for players"

  chan <- C.newChan
  let initialState = MkServerState serverSocket [] chan ZeroConnections 0
  acceptNext initialState >>= moveOverStates

moveOverStates :: ServerState -> IO ()
moveOverStates state = do
  msg <- C.readChan (clientChannel state)
  newState <- serverTrans state msg
  moveOverStates newState


