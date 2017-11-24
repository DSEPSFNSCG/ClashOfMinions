module CardGame.Client where

import           CardGame.Game
import           CardGame.Types
import qualified Control.Concurrent     as C
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           System.IO              (Handle, hIsEOF)


clog :: Client -> String -> IO ()
clog client str = do
  putStrLn $ "Client " ++ show (clientId client) ++ ": " ++ str

clientHandler :: TVar ServerState -> Handle -> Int -> IO ()
clientHandler serverState handle cid = do
  state <- atomically $ newTVar Unknown
  clientLoop serverState $ MkClient { clientId = cid
                        , clientHandle = handle
                        , clientState = state }

clientLoop :: TVar ServerState -> Client -> IO ()
clientLoop serverState client = do
  request <- clientReceive client
  trans serverState client request
  case request of
    Just _  -> clientLoop serverState client
    Nothing -> return ()


clientReceive :: Client -> IO (Maybe Request)
clientReceive client = do
  isEOF <- hIsEOF $ clientHandle client
  if isEOF
    then return Nothing
    else do
      line <- BS.hGetLine $ clientHandle client
      case decode (fromStrict line) of
        Nothing -> do
          tell client $ ErrorReply { errorMessage = "Invalid request" }
          clientReceive client
        Just request -> return $ Just request

trans :: TVar ServerState -> Client -> Maybe Request -> IO ()
trans serverSt client (Just NewGame) = do
  maybeGame <- atomically $ queue serverSt client
  case maybeGame of
    Nothing   -> clog client "You are queued"
    Just game -> do
      C.forkIO $ runGame game
      return ()
trans serverSt client Nothing = do
  atomically $ unqueue serverSt client
  clog client "You are unqueued"
trans serverSt client (Just request) = do
  result <- atomically $ do
    state <- readTVar $ clientState client
    case state of
      Unknown -> return $ Left "Invalid request at this time"
      WaitingNewGame -> return $ Left "Invalid request at this time"
      InGame game -> do
        writeTQueue (events game) (client, (RequestT request))
        return $ Right "success"
  case result of
    Left error -> clog client error
    Right succ -> clog client succ


queue :: TVar ServerState -> Client -> STM (Maybe Game)
queue serverSt client = let clientSt = clientState client in do
  state <- readTVar clientSt
  case state of
    Unknown -> do
      MkServerState games inQueue <- readTVar serverSt
      case inQueue of
        Nothing -> do
          writeTVar clientSt WaitingNewGame
          writeTVar serverSt (MkServerState games (Just client))
          return Nothing
        Just pairClient -> do
          if client == pairClient then return Nothing else do
            let gameId = length games
            game <- newGame gameId client pairClient
            writeTVar clientSt $ InGame game
            writeTVar (clientState pairClient) $ InGame game
            writeTVar serverSt $ MkServerState (game:games) Nothing
            return $ Just game
    _ -> return Nothing

unqueue :: TVar ServerState -> Client -> STM ()
unqueue serverSt client = let clientSt = clientState client in do
  state <- readTVar clientSt
  case state of
    WaitingNewGame -> do
      MkServerState games inQueue <- readTVar serverSt
      case inQueue of
        Just pairClient -> do
          if client /= pairClient then return () else do
            writeTVar serverSt $ MkServerState games Nothing
        Nothing -> return ()
    Unknown -> return ()
    InGame game -> do
      writeTQueue (events game) (client, Disconnect)
      return ()


