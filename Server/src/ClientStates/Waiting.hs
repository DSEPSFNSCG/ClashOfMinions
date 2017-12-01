module ClientStates.Waiting where

import           Client
import           Client.Types
import           ClientStates.Generic.Types
import           ClientStates.Lobby
import           ClientStates.Waiting.Types
import           Control.Concurrent
import           Control.Concurrent.STM          (STM, TVar, atomically,
                                                  readTVar, writeTVar)
import           Control.Monad.STM.Class
import           Control.Monad.Trans.Writer.Lazy
import           Data.Aeson
import           Data.Maybe
import           Game
import           Types

{--type Params c = ClientQueue c -> ClientQueue c -> ServerState

class Transition a where
  onConnect :: ClientT c => a c req resp -> Params c -> c -> ([(c, Response resp)], ServerState)
  onRequest :: ClientT c => a c req resp -> Params c -> c -> Request req -> ([(c, Response resp)], ServerState)
  onDisconnect :: ClientT c => a c req resp -> Params c -> c -> ([(c, Response resp)], ServerState)
  --}


waitingLoop :: (ClientT c) => ClientQueue c -> ClientQueue c -> TVar (ServerState c) -> IO ()
waitingLoop waitingQueue lobbyQueue serverState = do
  ((responses, maybeIO), logStrings) <- atomically $ do
    (client, event) <- getEvent waitingQueue
    runWriterT $ waitingTrans waitingQueue lobbyQueue serverState client event

  putStrLn "Hi there!"
  mapM_ putStrLn logStrings
  mapM_ (uncurry send) responses
  forkIO $ fromMaybe (return ()) maybeIO

  waitingLoop waitingQueue lobbyQueue serverState


type TransResult c = WriterT [String] STM ( [(c, Response WaitingResponse)] , Maybe (IO ()))

logResp :: ClientT c => c -> String -> TransResult c
logResp c s = do
  tell ["Telling client " ++ show c ++ ": " ++ s]
  return ([(c, Left (LogResponse { responseString = s }))], Nothing)

resp :: ClientT c => c -> Response WaitingResponse -> TransResult c
resp c r = do
  tell ["Telling client " ++ show c ++ ": " ++ show r]
  return ([(c, r)], Nothing)


logRequest :: ClientT c => c -> Request WaitingRequest -> TransResult c
logRequest client request = do
  tell ["Client " ++ show client ++ " sent request: " ++ show request]
  return ([], Nothing)

waitingTrans :: ClientT c => ClientQueue c -> ClientQueue c -> TVar (ServerState c) -> c -> Event (Maybe (Request WaitingRequest)) -> TransResult c
waitingTrans _ _ _ client Connect         = logResp client "You are connected!"
waitingTrans _ _ _ client (Input Nothing) = logResp client "Invalid request!"
waitingTrans _ _ _ client Disconnect      = logResp client "You are disconnected!"
waitingTrans w l serverState client (Input (Just (Left _))) = logResp client "Areeee"
waitingTrans w l serverState client (Input (Just (Right (NewGame { name = name })))) = do
  state <- liftSTM $ readTVar serverState
  case inQueue state of
    Nothing -> do
      liftSTM $ writeTVar serverState $ state { inQueue = Just (client, name) }
      liftSTM $ setReceiver client l
      tell ["now 1 client in lobby"]
      logResp client "Queued!"
      return ([(client, Right Queued)], Nothing)
    Just (client', name') -> if client' == client then logResp client "already queued" else do
      liftSTM $ writeTVar serverState $ state { inQueue = Nothing }
      let gameId = length $ games state
      gameIO <- liftSTM $ newGame gameId client name client' name'
      tell ["Created new game!"]
      return ([(client, Right Queued)], Just gameIO)
waitingTrans w l serverState client (Input (Just (Right (RestoreGame r)))) = do
  state <- liftSTM $ readTVar serverState
  if gameID r < length (games state) then
    resp client $ Right $ RestoreGameResponse InvalidGameID else
    resp client $ Right $ RestoreGameResponse RestoreSuccess



