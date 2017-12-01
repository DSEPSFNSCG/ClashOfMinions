module Game where

import           Client
import           Client.Types
import           ClientStates.Generic
import           ClientStates.Generic.Types
import           ClientStates.Lobby.Types
import           ClientStates.Waiting.Types
import           Data.Word
import           Game.Types

import           Data.Tuple
import           Types

import           Control.Concurrent         (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import           GHC.Generics
import qualified Network.Socket             as NS
import           System.IO                  (Handle, hClose, hIsEOF)
import           System.Random              (RandomGen, random, randomIO)

playerSend :: ClientT c => Player c -> Response GameResponse -> IO ()
playerSend p resp = clientSend (playerClient p) (encodeResponse resp)

broadcast :: ClientT c => Game c -> Response GameResponse -> IO ()
broadcast game resp = do
  playerSend (currentPlayer game) resp
  playerSend (waitingPlayer game) resp


newPlayer :: ClientT c => c -> String -> Word64 -> Player c
newPlayer c name token = MkPlayer { playerClient = c
                                        , playerName = name
                                        , playerToken = token
                                        , playerState = () }

--restoreGame :: ClientT c => c -> RestoreGameRequest -> Either RestoreGameResult

newGame :: ClientT c => GameID -> c -> String -> c -> String -> STM (IO ())
newGame id c1 n1 c2 n2 = do
  queue <- newTQueue
  setReceiver c1 queue
  setReceiver c2 queue
  return $ do
    r <- randomIO
    t1 <- randomIO
    t2 <- randomIO
    let p1 = newPlayer c1 n1 t1
    let p2 = newPlayer c2 n2 t2
    let game = MkGame { gameId = id
                      , currentPlayer = if r then p1 else p2
                      , waitingPlayer = if r then p2 else p1
                      , gameState = ()
                      , gameQueue = queue }
    putStrLn "Started game"
    gameLoop game
    close c1
    close c2

foo :: Event (Maybe (Request GameRequest)) -> IO Int
foo m = return 0

gameLoop :: ClientT c => Game c -> IO ()
gameLoop game = do
  (client, message) <- atomically $ getEvent (gameQueue game)
  foo message
  putStrLn "Got message"
  gameLoop game

