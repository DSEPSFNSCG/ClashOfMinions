module Pair where

import           Client
import           Client.Types
import           Control.Concurrent
import           Control.Concurrent.STM          (STM, TVar, atomically,
                                                  readTVar, writeTVar)
import           Control.Concurrent.STM          (STM, TVar)
import           Control.Monad.STM.Class         (liftSTM)
import           Control.Monad.Trans.Writer.Lazy (tell)
import           Data.Maybe
import           Pair.Types
import           Types


pairingTrans :: Server -> ServerState -> Client -> Maybe LobbyRequest -> TransResult LobbyResponse ()

pairingTrans srv s@(ServerState { inQueue = Just (c', _) }) c r = case c == c' of
  True -> do
    let newState = s { inQueue = Nothing }
    case r of
      Nothing -> do
        tell $ "[SERVER] Client waiting for new game disconnected"
        return (newState, Nothing)
      Just _ -> do
        tell $ "[SERVER] Client waiting for new game sent unqueue request"
        liftSTM $ setReceiver c (initQueue srv)
        return (newState, Just (Left Unqueued))
  False -> return (s, Nothing)
pairingTrans _ s _ _ = return (s, Nothing)

