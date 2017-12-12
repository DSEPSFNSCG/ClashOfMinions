module Game where

import           Client
import           Client.Types
import           Control.Concurrent              (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Monad.STM.Class
import           Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString                 as BS
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Matrix
import           Data.Maybe
import           Data.Tuple
import           Debug.Trace
import           Game.Types
import           GameLogic
import           GHC.Generics
import qualified Network.Socket                  as NS
import           System.IO                       (Handle, hClose, hIsEOF)
import           System.Random                   (RandomGen, random, randomIO,
                                                  randomR)
import           Types

randomToken :: RandomGen g => g -> Int -> (Token, g)
randomToken g 0 = ([], g)
randomToken g length =
  let (c, g') = randomR ('a','z') g
      (cs, g'') = randomToken g' (length - 1)
  in (c : cs, g'')

makePlayer :: RandomGen g => g -> Bool -> Client -> String -> WriterT String STM (Player, g)
makePlayer g left client name = do
  let (token, g') = randomToken g 16
  tell $ "Token of " ++ if left then "left" else "Rgiht" ++ " player is " ++ token
  clientVar <- liftSTM $ newTVar $ Just client
  return $ (MkPlayer { playerClient = clientVar
                    , playerName = name
                    , playerToken = token
                    , leftPlayer = left }, g')

makeGame :: RandomGen g => g -> Int -> (Client, String) -> (Client, String) -> WriterT String STM (Game, g)
makeGame g gid (c, n) (c', n') = do
  let (p1Starts, g') = random g
  (p, g'') <- makePlayer g' p1Starts c n
  (p', g''') <- makePlayer g'' (not p1Starts) c' n'
  let state = uncurry initialGameState $ if p1Starts then (p, p') else (p', p)

  tqueue <- liftSTM newTQueue
  queue <- liftSTM newTQueue
  stateVar <- liftSTM $ newTVar state

  liftSTM $ setReceiver c tqueue
  liftSTM $ setReceiver c' tqueue

  return $ (MkGame { gameId = gid
                   , p1 = p
                   , p2 = p'
                   , gameState = stateVar
                   , transQueue = tqueue
                   , gameQueue = queue
                   }, g''')

toGameEvent :: Maybe GameRequest -> GameEvent
toGameEvent Nothing        = PlayerDisconnect
toGameEvent (Just request) = PlayerRequest request

gameClientHandler :: Game -> ByteQueue -> GameQueue -> IO ()
gameClientHandler game bytes queue = do
  result <- atomically $ do
    (client, message) <- getRequest bytes
    mplayer <- playerForClient game client
    case mplayer of
      Nothing -> return Nothing
      Just player -> case message of
        Just msg -> do
          writeTQueue queue (player, toGameEvent msg)
          return Nothing
        Nothing -> return $ Just $ do
          sendResponse client invalidGameRequest

  fromMaybe (return ()) result
  gameClientHandler game bytes queue


runGame :: Game -> IO ()
runGame game = do
  forkIO $ gameClientHandler game (transQueue game) (gameQueue game)
  anounceStart game

  gameLoop game

  (c, c') <- atomically $ do
    c <- readTVar (playerClient (p1 game))
    c' <- readTVar (playerClient (p2 game))
    return (c, c')

  maybe (return ()) close c
  maybe (return ()) close c'



anounceStart :: Game -> IO ()
anounceStart game = do
  state <- atomically $ readTVar (gameState game)
  let (p, p') = (currentPlayer state, waitingPlayer state)
  _ <- playerSend p $ GameStart { f_gameId = (gameId game)
                                , f_youStart = True
                                , f_otherName = playerName p'
                                , f_token = playerToken p
                                }
  _ <- playerSend p' $ GameStart { f_gameId = (gameId game)
                                 , f_youStart = False
                                 , f_otherName = playerName p
                                 , f_token = playerToken p'
                                 }
  return ()


matchToken :: Game -> Client -> Token -> WriterT String STM (Maybe Player)
matchToken game client token = do
  tell $ "Game has tokens " ++ (playerToken $ p1 game) ++ " and " ++ (playerToken $ p2 game)
  if (playerToken $ p1 game) == token then return $ Just $ p1 game
  else if (playerToken $ p2 game) == token then return $ Just $ p2 game
  else return Nothing

-- Restores a game if the token belongs to a player of this game
restore :: Game -> Client -> Token -> Maybe Int -> WriterT String STM (Maybe History)
restore game client token historyFrom = do
  res <- matchToken game client token
  case res of
    Nothing -> return Nothing
    Just player -> do
      liftSTM $ writeTVar (playerClient player) $ Just client
      liftSTM $ setReceiver client (transQueue game)
      liftSTM $ writeTQueue (gameQueue game) (player, PlayerReconnect)
      state <- liftSTM $ readTVar $ gameState game
      let all = history state
      return $ Just $ drop (fromMaybe 0 historyFrom) (reverse all)

playerForClient :: Game -> Client -> STM (Maybe Player)
playerForClient game client = do
  client1 <- readTVar $ playerClient (p1 game)
  if Just client == client1 then
    return $ Just (p1 game) else do
      client2 <- readTVar $ playerClient (p2 game)
      if Just client == client2 then
        return $ Just (p2 game) else
        return Nothing


gameLoop :: Game -> IO ()
gameLoop game = do
  (player, event, state) <- atomically $ do
    (player, event) <- readTQueue (gameQueue game)
    state <- readTVar (gameState game)
    return (player, event, state)

  newState <- gameTransition game state player event

  atomically $ writeTVar (gameState game) newState
  gameLoop game










