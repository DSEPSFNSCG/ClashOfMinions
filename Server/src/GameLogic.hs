module GameLogic where

import           Client
import           Control.Concurrent.STM (readTVarIO)
import           Data.Matrix
import           Game.Types

-- Tries to send a message to a player, returns False if the player is currently disconnected
playerSend :: Player -> GameResponse -> IO Bool
playerSend player response = do
  client <- readTVarIO (playerClient player)
  case client of
    Nothing -> return False
    Just c -> do
      sendResponse c response
      return True

initialGameState :: Player -> Player -> GameState
initialGameState currentPlayer waitingPlayer =
  MkGameState { history = []
              , field = matrix 4 10 (const Nothing)
              , currentPlayer = currentPlayer
              , waitingPlayer = waitingPlayer
              }

gameTransition :: GameState -> Player -> GameEvent -> IO GameState
gameTransition gameState player (PlayerRequest (Place placement)) = do
  putStrLn $ "[GAME] Received placement: " ++ show placement
  playerSend player $ LogResponse "Is it your turn?"
  return gameState
gameTransition gameState player PlayerDisconnect = do
  putStrLn $ "[GAME] Player disconnected!"
  return gameState
gameTransition gameState player PlayerReconnect = do
  putStrLn $ "[GAME] Player reconnected!"
  playerSend player $ LogResponse "You have been reconnected!"
  return gameState
