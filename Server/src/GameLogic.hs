module GameLogic where

import           Client
import           Control.Concurrent.STM (readTVarIO)
import           Data.List
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
              , nextMinionId = 0
              }

simulate :: Bool -> Field -> Field
simulate left field = field -- TODO

switchPlayers :: GameState -> GameState
switchPlayers state@(MkGameState { currentPlayer = c, waitingPlayer = w }) =
  state { currentPlayer = w
        , waitingPlayer = c
        }

doPlacement :: Game -> GameState -> Player -> Placement -> Either String GameState
doPlacement game state player placement@(MkPlacement { f_position = p, f_stats = s}) =
  let
      c = fst p + 1
      r = snd p + 1
      conditions =
        [ if leftPlayer player then
            (0 < c && c <= 4, "X must be between 1 and 4") else
            (6 < c && c <= 10, "X must be between 7 and 10")
        , (0 < r && r <= 4, "Y must be between 1 and 4")
        , (f_health s < 10, "health must be below 10")
        , (f_boosthealrange s < 10, "boostrange must be below 10")
        , (f_attackrange s < 10, "attackrange must be below 10")
        , (f_attackdmg s < 10, "attackdmg must be below 10")
        , (f_healstrength s < 10, "healstrength must be below 10")
        , (getElem r c (field state) == Nothing, "field is already occupied")
        ]

      error = fmap snd . find (\(p, e) -> not p) $ conditions

      minion = MkMinion { minionId = nextMinionId state
                        , minionStats = s
                        , owner = player
                        }
      newField = setElem (Just minion) (r, c) $ field state
  in case error of
    Just msg -> Left msg
    Nothing -> Right $ switchPlayers $ state
     { history = placement:(history state)
     , field = simulate (leftPlayer player) newField
     , nextMinionId = (nextMinionId state) + 1
     }


gameTransition :: Game -> GameState -> Player -> GameEvent -> IO GameState
gameTransition game gameState player (PlayerRequest (Place placement)) = do
  putStrLn $ "[GAME] Received placement: " ++ show placement
  if player /= (currentPlayer gameState) then do
    putStrLn $ "[GAME] non-current player wanted to place"
    playerSend player NotYourTurn
    return gameState
  else case doPlacement game gameState player placement of
    Left error -> do
      playerSend player $ InvalidPlacing error
      putStrLn $ "[GAME] Invalid placement: " ++ error
      return gameState
    Right newGameState -> do
      playerSend player $ PlaceSuccess
      putStrLn $ "[GAME] Successful placement!"
      putStrLn $ "[GAME] Gamestate is now: " ++ simpleShow newGameState
      --putStrLn $ "\n[GAME] Full Gamestate: " ++ show newGameState
      return newGameState
gameTransition game gameState player PlayerDisconnect = do
  putStrLn $ "[GAME] Player disconnected!"
  return gameState
gameTransition game gameState player PlayerReconnect = do
  putStrLn $ "[GAME] Player reconnected!"
  playerSend player $ LogResponse "You have been reconnected!"
  return gameState
