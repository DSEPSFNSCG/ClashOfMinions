module Game (newGame
            , Game
            , runGame
            ) where

import           Data.Tuple
import           Types

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Data.Aeson
import qualified Data.ByteString         as BS
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import qualified Network.Socket          as NS
import           System.IO               (Handle, hClose, hIsEOF)
import           System.Random           (RandomGen, random, randomIO)

broadcast :: Game -> Reply -> IO ()
broadcast (MkGame { players = (p1, p2) }) str = do
  tell p1 str
  tell p2 str


newPlayer :: Client -> Player
newPlayer c = MkPlayer { playerHandle = clientHandle c
                       , playerState = Waiting }

newGame :: GameID -> Client -> Client -> STM Game
newGame id c1 c2 = do
  queue <- newTQueue
  return $ MkGame
    { gameId = id
    , players = (newPlayer c1, newPlayer c2)
    , gamestate = ()
    , events = queue }

runGame :: Game -> IO ()
runGame game = do
  putStrLn "Running game test"
  putStrLn "Broadcasting something"
  broadcast game $ Log { message = "something" }


gameLoop :: Game -> IO ()
gameLoop game = do
  putStrLn "New game!"
  starts <- randomIO :: IO Bool
  putStrLn $ "Player " ++ (if starts then "1" else "2") ++ " starts"
  let actualGame = game {
    players = if starts then players game else swap (players game)
  }
  --tell (fst (players actualGame)) (Log { message = "hi p1" })
  --tell (snd (players actualGame)) (Log { message = "hi p2" })
  hClose (handle (fst (players actualGame)))
  hClose (handle (snd (players actualGame)))


handleEvent :: Chan (Player, Request) -> IO ()
handleEvent chan = do
  event <- readChan chan
  putStrLn "Something happened"
  handleEvent chan
