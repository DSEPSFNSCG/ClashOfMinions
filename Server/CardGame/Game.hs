module CardGame.Game (newGame
                     , Game
                     ) where

import           CardGame.Types
import           Data.Tuple

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan
import           Data.Aeson
import qualified Data.ByteString         as BS
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import qualified Network.Socket          as NS
import           System.IO               (Handle, hClose, hIsEOF)
import           System.Random           (RandomGen, random, randomIO)


data Game = MkGame
  { players   :: (Player, Player)
  , gamestate :: () } deriving (Show)

data PlayerState = Waiting deriving (Show)

data Player = MkPlayer {
  handle      :: Handle,
  playerState :: PlayerState
} deriving (Show)


newGame :: GameID -> Client -> Client -> Chan (Client, ClientMessage) -> IO Game
newGame id c1 c2 chan = do
  putStrLn $ "Starting game with id " ++ show id
  let p1 = MkPlayer {
        handle = clientHandle c1,
        playerState = Waiting
        }
  let p2 = MkPlayer {
        handle = clientHandle c2,
        playerState = Waiting
        }
  let game = MkGame { players = (p1, p2)
                    , gamestate = ()
                    }
  forkIO $ gameLoop game
  return game


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
