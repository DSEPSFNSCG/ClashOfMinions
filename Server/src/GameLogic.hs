module GameLogic where

import           Client
import           Control.Concurrent.STM (readTVarIO)
import           Control.Monad
import           Data.List
import           Data.Matrix
import qualified Data.Vector            as V
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


simulate :: Bool -> Player -> Field -> Maybe Field
simulate True player field = simulateLeft player field
simulate False player field = fmap turnField . simulateLeft player . turnField $ field

turnField :: Field -> Field
turnField field =
 let
   colsField = map (flip getCol field) [0..9]
   colsFieldReverse = reverse colsField
  in
   matrix 4 10 $ \(r, c) -> V.unsafeIndex (colsFieldReverse !! (c - 1)) r

simulateLeft :: Player -> Field -> Maybe Field
simulateLeft player field = do
  moved <- doMovement player field
  return moved

doMovement :: Player -> Field -> Maybe Field
doMovement player field = foldM (doColumnMovement player) field [9,8..0]

doColumnMovement :: Player -> Field -> Int -> Maybe Field
doColumnMovement player field columnIndex = Just field







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
        [ (if leftPlayer player then
            (0 <= c && c <= 4, "X must be between -1 and 3") else
            (c == 0 || 6 < c && c <= 10, "X must be between 6 and 9 or -1"))
        , (0 <= r && r <= 4, "Y must be between -1 and 3")
        , (f_attackdmg s < 5 , "You cheater!")
        , (f_attackrange s < 4, "You cheater!")
        , (f_buffrange s < 4 , "You cheater!")
        , (f_healing s < 5, "You cheater!")
        , (f_atkbuff s < 5, "You cheater!")
        , (f_healbuff s < 5, "You cheater!")
        , (f_shield s < 5, "You cheater!")
        , (f_maxhealth s < 5, "You cheater!")

        --, (f_attackdmg s + f_attackrange s + f_buffrange s + f_healing s + f_atkbuff s + f_healbuff s + f_shield s + f_maxhealth s < 5, "Too many points!")

        ]

      error = fmap snd . find (\(p, e) -> not p) $ conditions

      minion = MkMinion { minionId = nextMinionId state
                        , minionStats = s
                        , owner = player
                        }
      newField = setElem (Just minion) (r, c) $ field state
  in case error of
    Just msg -> Left msg
    Nothing -> Right $
     if c == 0 || r == 0 then
       state { history = placement:(history state)
             , currentPlayer = (waitingPlayer state)
             , waitingPlayer = (currentPlayer state)
             }
     else
       state { history = placement:(history state)
             , nextMinionId = (nextMinionId state) + 1
             , currentPlayer = (waitingPlayer state)
             , waitingPlayer = (currentPlayer state)
             }
    {-
      case simulate (leftPlayer player) player newField of
      Nothing -> return $ GameDone { winningPlayer = player
                                   , losingPlayer = (waitingPlayer state) }
      Just newField' -> return $ switchPlayers $
        state { history = placement:(history state)
              , field = newField'
              , nextMinionId = (nextMinionId state) + 1 }-}


gameTransition :: Game -> GameState -> Player -> GameEvent -> IO GameState
gameTransition _ state@(GameDone _ _) _ _ = return state
gameTransition game gameState player (PlayerRequest (Place placement)) = do
  putStrLn $ "[GAME] Received placement: " ++ show placement
  if player /= (currentPlayer gameState) then do
    putStrLn $ "[GAME] non-current player wanted to place"
    playerSend player NotYourTurn
    return gameState
  else case doPlacement game gameState player placement of
    Left error -> do
      putStrLn $ "[GAME] Invalid placement: " ++ error ++ ", giving up"
      playerSend player $ InvalidPlacing error
      gameTransition game gameState player (PlayerRequest GiveUp)
    Right newGameState -> do
      playerSend player $ PlaceSuccess
      playerSend (waitingPlayer gameState) $ OtherPlayerPlaced placement
      putStrLn $ "[GAME] Successful placement!"
      putStrLn $ "[GAME] Gamestate is now: " ++ simpleShow newGameState
      --putStrLn $ "\n[GAME] Full Gamestate: " ++ show newGameState
      return newGameState
gameTransition game gameState player (PlayerRequest GiveUp) = do
  putStrLn $ "[GAME] Player gave up!"
  if player == (p1 game)
    then do
      playerSend (p2 game) OtherPlayerGaveUp
      return $ GameDone { winningPlayer = p2 game
                        , losingPlayer = p1 game }
    else do
      playerSend (p1 game) OtherPlayerGaveUp
      return $ GameDone { winningPlayer = p1 game
                        , losingPlayer = p2 game }
gameTransition game gameState player PlayerDisconnect = do
  putStrLn $ "[GAME] Player disconnected!"
  return gameState
gameTransition game gameState player PlayerReconnect = do
  putStrLn $ "[GAME] Player reconnected!"
  playerSend player $ GameLogResponse "You have been reconnected!"
  return gameState
