module Server where

import           Client
import           Client.Types
import           Game
import           Game.Types
import           Pair
import           Pair.Types
import           Types


import           Control.Concurrent              (forkIO)
import           Control.Concurrent.STM
import           Control.Exception               (SomeException, handle)
import           Control.Monad.STM.Class
import           Control.Monad.Trans.Writer.Lazy (runWriterT, tell)
import           Data.Aeson
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Maybe                      (fromMaybe, listToMaybe)
import           Network                         (PortID, listenOn)
import qualified Network.Socket                  as S
import           System.Environment              (getArgs)
import           System.IO                       (BufferMode (LineBuffering),
                                                  IOMode (ReadWriteMode),
                                                  hIsEOF, hSetBuffering, stdin,
                                                  stdout)
import           Text.Read                       (readMaybe)

server :: PortID -> IO ()
server port = do
  -- Apparently required to get instant output with systemd
  hSetBuffering stdout LineBuffering

  socket <- listenOn $ port
  putStrLn $ "[SERVER] Listening on port " ++ show port ++ ", waiting for players"

  handle (onInterrupt socket) $ runOnSocket socket

onInterrupt :: S.Socket -> SomeException -> IO ()
onInterrupt socket e = do
  putStrLn "[SERVER] Interrupted, closing socket"
  S.close socket


runOnSocket :: S.Socket -> IO ()
runOnSocket socket = do
  initQueue <- atomically $ newTQueue
  waitQueue <- atomically $ newTQueue
  state <- initialServerState
  stateVar <- atomically $ newTVar state
  let server = Server { initQueue = initQueue
                      , waitQueue = waitQueue
                      , serverStateVar = stateVar
                      }

  forkIO $ loop server initQueue InvalidWaitingRequest id waitingTrans
  forkIO $ loop server waitQueue InvalidPairingRequest return pairingTrans
  acceptConnections initQueue socket 0

acceptConnections :: ByteQueue -> S.Socket -> Int -> IO ()
acceptConnections queue socket nextId = do
  (clientSocket, _) <- S.accept socket
  handle <- S.socketToHandle clientSocket ReadWriteMode
  hSetBuffering handle LineBuffering
  putStrLn $ "Client " ++ show nextId ++ " connected!"
  forkIO $ handleClient queue handle nextId
  acceptConnections queue socket $ nextId + 1

loop :: (FromJSON q, ToJSON r, ToJSON q) => Server -> ByteQueue -> r -> (a -> IO ()) -> (Server -> ServerState -> Client -> Maybe q -> TransResult r a) -> IO ()
loop server queue invalid g f = do
  (client, result, log) <- atomically $ do
    (client, req) <- getRequest queue
    case req of
      Just r -> do
        state <- readTVar (serverStateVar server)
        ((newState, result), log) <- runWriterT $ f server state client r
        writeTVar (serverStateVar server) newState
        return (client, result, "Client sent request " ++ show (toStrict $ encode r) ++ "\n\t" ++ log)
      Nothing -> do
        return (client, Just (Left invalid), "Invalid request")

  putStrLn $ "[SERVER] Client " ++ show client ++ ": " ++ log

  case result of
    Nothing              -> return ()
    Just (Left response) -> sendResponse client response
    Just (Right a)       -> g a

  loop server queue invalid g f




waitingTrans :: Server -> ServerState -> Client -> Maybe WaitingRequest -> TransResult WaitingResponse (IO ())

waitingTrans _ s c Nothing = return (s, Nothing)
waitingTrans srv s@(ServerState { inQueue = Nothing }) c (Just (NewGame { name = n })) = do
  tell $ "Requested new game, that's one client"
  liftSTM $ setReceiver c (waitQueue srv)
  return (s { inQueue = Just (c, n) }, Just $ Left $ Queued)
waitingTrans _ s@(ServerState { inQueue = Just cn', games = games, rng = rng }) c (Just (NewGame { name = n })) = do
  tell $ "Requested new game, that's two clients!"
  let gid = length games
  (game, rng') <- liftSTM $ makeGame rng gid (c, n) cn'
  let io = do
        _ <- forkIO $ runGame game
        return ()
  return (s { inQueue = Nothing, games = game:games, rng = rng' }, Just $ Right $ io)
waitingTrans _ s@(ServerState { games = gs }) c (Just (RestoreGame (RestoreGameRequest { g_gameId = i, g_token = t, g_historyFrom = h }))) = do
  tell $ "Wants to restore game with id " ++ show i ++ ", token " ++ show t ++ ", historyFrom " ++ show h
  case i < length gs of
    False -> return (s, Just $ Left InvalidGameID)
    True -> do
      let g = gs !! i
      mhist <- liftSTM $ restore g c t h
      case mhist of
        Nothing   -> return (s, Just $ Left TokenMismatch)
        Just hist -> return (s, Just $ Left $ RestoreSuccess hist)
