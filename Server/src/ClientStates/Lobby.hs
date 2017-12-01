module ClientStates.Lobby where

import           Client
import           Client.Types
import           ClientStates.Generic.Types
import           ClientStates.Lobby.Types
import           ClientStates.Waiting.Types
import           Control.Concurrent
import           Control.Concurrent.STM          (STM, TVar, atomically,
                                                  readTVar, writeTVar)
import           Control.Concurrent.STM          (STM, TVar)
import           Control.Monad.STM.Class
import           Control.Monad.Trans.Writer.Lazy
import           Data.Maybe
import           Types

lobbyLoop :: (ClientT c) => ClientQueue c -> ClientQueue c -> TVar (ServerState c) -> IO ()
lobbyLoop waitingQueue lobbyQueue serverState = do
  ((responses, maybeIO), logStrings) <- atomically $ do
    (client, event) <- getEvent lobbyQueue
    runWriterT $ lobbyTrans waitingQueue lobbyQueue serverState client event

  putStrLn "This is in the lobby loop"
  mapM_ putStrLn logStrings
  mapM_ (uncurry send) responses
  forkIO $ fromMaybe (return ()) maybeIO

  lobbyLoop waitingQueue lobbyQueue serverState


type LTransResult c = WriterT [String] STM ( [(c, Response LobbyResponse)] , Maybe (IO ()))

llogResp :: ClientT c => c -> String -> LTransResult c
llogResp c s = do
  tell ["Telling client " ++ show c ++ ": " ++ s]
  return ([(c, Left (LogResponse { responseString = s }))], Nothing)

llogRequest :: ClientT c => c -> Request WaitingRequest -> LTransResult c
llogRequest client request = do
  tell ["Client " ++ show client ++ " sent request: " ++ show request]
  return ([], Nothing)

lobbyTrans :: ClientT c => ClientQueue c -> ClientQueue c -> TVar (ServerState c) -> c -> Event (Maybe (Request LobbyRequest)) -> LTransResult c
lobbyTrans w l serverState client (Input (Just (Right Unqueue))) = do
  state <- liftSTM $ readTVar serverState
  case inQueue state of
    Nothing -> do
      tell ["Weird, client " ++ show client ++ "was able to send unqueue even though he's not registered as queued"]
      liftSTM $ setReceiver client w
      return ([(client, Right Unqueued)], Nothing)
    Just (client', _) -> if client' == client then do
      liftSTM $ writeTVar serverState $ state { inQueue = Nothing }
      liftSTM $ setReceiver client w
      tell ["Unqueued client " ++ show client]
      return ([(client, Right Unqueued)], Nothing)
      else do
        tell ["Weird, client " ++ show client ++ "was able to send unqueue even though he's not registered as queued"]
        return ([], Nothing)
lobbyTrans w l serverState client _ = llogResp client "Hi there!"
