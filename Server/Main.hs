module Main where


import           CardGame.Game
import           CardGame.Types

import qualified Control.Concurrent as C
import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Network            as N
import qualified Network.Socket     as NS
import           System.Environment (getArgs)
import           System.IO          (BufferMode (LineBuffering), Handle,
                                     IOMode (ReadWriteMode), hGetLine, hIsEOF,
                                     hPutStrLn, hSetBuffering, stdout)
import           System.Random      (randomIO)
import           Text.Read          (readMaybe)


data ServerState = MkServerState { connections :: ServerConnectionState
                                 , games       :: [Game]
                                 } deriving (Show)

data ServerConnectionState = ZeroConnections
                           | OneConnection (C.ThreadId, Client)
                           deriving (Show)

data ServerMessage = NewConnection (C.ThreadId, Client)
                   | Disconnected (C.ThreadId, Client)
                   deriving (Show)

serverTrans :: ServerState -> ServerMessage -> IO ServerState
serverTrans state@(MkServerState { connections = ZeroConnections }) (NewConnection c) = do
  putStrLn "[SERVERSTATE] Got a first connection in queue"
  return $ state { connections = OneConnection c }
serverTrans state@(MkServerState { connections = ZeroConnections }) (Disconnected _) = do
  putStrLn "[SERVERSTATE] Possible bug: Somebody not in the queue disconnected!"
  return $ state
serverTrans state@(MkServerState { connections = OneConnection c, games = games }) (NewConnection conC) = do
  putStrLn "[SERVERSTATE] Got a second connection in queue, starting game.."
  C.killThread $ fst c
  C.killThread $ fst conC
  let gameId = length games
  game <- newGame gameId (snd c) (snd conC)
  return $ MkServerState { connections = ZeroConnections, games = game:games }
serverTrans state@(MkServerState { connections = OneConnection c }) (Disconnected discC) =
  if (c == discC)
  then do
    putStrLn "[SERVERSTATE] Somebody disconnected, queue empty"
    return $ state { connections = ZeroConnections }
  else do
    putStrLn "[SERVERSTATE] Possible bug: Somebody not in the queue disconnected!"
    return $ state { connections = OneConnection c }

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

  -- Set up channel to send client connect/disconnect messages
  serverChannel <- C.newChan
  -- Handle new client connections on another thread
  C.forkIO $ acceptConnections 0 serverChannel serverSocket
  -- Handle channel messages on main thread
  serverMessageLoop serverChannel (MkServerState { connections = ZeroConnections, games = [] })

-- Forever handles a message on the channel and transitions the server state accordingly
serverMessageLoop :: C.Chan ServerMessage -> ServerState -> IO ()
serverMessageLoop serverChannel state = do
  message <- C.readChan serverChannel
  newState <- serverTrans state message
  serverMessageLoop serverChannel newState

acceptConnections :: Int -> C.Chan ServerMessage -> NS.Socket -> IO ()
acceptConnections nextClientId serverChannel serverSocket = do
  (clientSocket, address) <- NS.accept serverSocket
  handle <- NS.socketToHandle clientSocket ReadWriteMode
  hSetBuffering handle LineBuffering

  let client = MkClient { clientId = nextClientId
                        , clientHandle = handle
                        , clientAddress = address }
  putStrLn $ "New client connected: " ++ show client
  hPutStrLn (clientHandle client) "You are connected!"
  threadId <- C.forkIO $ clientLobby serverChannel client
  C.writeChan serverChannel $ NewConnection (threadId, client)
  acceptConnections (nextClientId + 1) serverChannel serverSocket

clientLobby :: C.Chan ServerMessage -> Client -> IO ()
clientLobby serverChannel client = do
  isEOF <- hIsEOF (clientHandle client)
  if isEOF
    then do
      putStrLn $ "Client disconnected: " ++ show client
      threadId <- C.myThreadId
      C.writeChan serverChannel (Disconnected (threadId, client))
    else do
      line <- hGetLine (clientHandle client)
      putStrLn $ "Received line " ++ line ++ " from client " ++ show client
      hPutStrLn (clientHandle client) "Game not started yet, hold on"
      clientLobby serverChannel client
