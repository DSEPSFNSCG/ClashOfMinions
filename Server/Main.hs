{-# LANGUAGE DeriveGeneric #-}

import System.Environment (getArgs)
import System.IO (
  IOMode( ReadWriteMode ),
  hSetBuffering,
  hPutStrLn,
  hGetLine,
  BufferMode ( LineBuffering ),
  Handle)
import qualified Network as N
import qualified Network.Socket as NS
import Network.Simple.TCP (
  HostPreference( HostAny ))
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Control.Concurrent (forkIO)

type State = Int

customOptions = defaultOptions {
  sumEncoding = defaultTaggedObject {
    tagFieldName = "type"
  }
}

data Request =
  Log {
    message :: String
  } |
  Test {
    hello :: String
  }
  | GetState
  deriving (Generic, Show)

instance ToJSON Request where
  toEncoding = genericToEncoding customOptions
instance FromJSON Request where
  parseJSON = genericParseJSON customOptions

data Reply =
  ErrorReply {
    errorMessage :: String
  }
  | ReturnState {
    returnState :: State
  }
  deriving (Generic, Show)

instance ToJSON Reply where
  toEncoding = genericToEncoding customOptions
instance FromJSON Reply where
  parseJSON = genericParseJSON customOptions


data PlayerState = Waiting
                 deriving (Show)

data Player = MkPlayer {
  handle :: Handle,
  address :: NS.SockAddr,
  state :: PlayerState
}

tell :: Player -> String -> IO ()
tell player = hPutStrLn (handle player)

ask :: Player -> IO String
ask player = hGetLine (handle player)

main = do
  args <- getArgs
  let port = fromIntegral (read $ head args :: Int)
  socket <- N.listenOn $ N.PortNumber port
  putStrLn $ "Listening on port " ++ show port ++ ", waiting for players"
  waitFirstPlayer socket

data Game = MkGame
  { p1 :: Player
  , p2 :: Player
  , gamestate :: () }

gameStep :: Game -> IO Game
gameStep game@(MkGame { p1=p1, p2=p2, gamestate=gamestate }) = do
  l1 <- ask p1
  tell p2 l1
  l2 <- ask p2
  tell p1 l2
  gameStep game

runGame :: Player -> Player -> IO ()
runGame a b = do
  let game = MkGame {
    p1 = a,
    p2 = b,
    gamestate = ()
  }
  tell a "Hi, you're player A"
  tell b "Hi, you're player B"
  gameStep game
  return ()

acceptConnection socket = do
  (clientSocket, addr) <- NS.accept socket
  handle <- NS.socketToHandle clientSocket ReadWriteMode
  hSetBuffering handle LineBuffering
  return (handle, addr)

waitFirstPlayer socket = do
  (handle, addr) <- acceptConnection socket
  let firstPlayer = MkPlayer {
      handle = handle
    , address = addr
    , state = Waiting }
  putStrLn $ "First player connected (" ++ show (address firstPlayer) ++ ")"
  tell firstPlayer $ "Connected, waiting for player 2"
  waitSecondPlayer firstPlayer socket

waitSecondPlayer firstPlayer socket = do
  (handle, addr) <- acceptConnection socket
  let secondPlayer = MkPlayer { handle = handle
    , address = addr
    , state = Waiting }
  putStrLn $ "Second player connected (" ++ show (address secondPlayer) ++ ")"
  tell secondPlayer $ "Connected"
  forkIO $ runGame firstPlayer secondPlayer
  waitFirstPlayer socket



  --handle <- NS.socketToHandle clientSocket ReadWriteMode
  --hSetBuffering handle LineBuffering

handleClient state handle = do
  strict <- BS.hGetLine handle
  case decode $ fromStrict strict :: Maybe Request of
    Nothing -> do
      putStrLn "invalid request"
      handleClient state handle
    Just message -> do
      let (reply, newstate) = handleRequest state message
      BS.hPutStrLn handle $ toStrict (encode reply)
      handleClient newstate handle

handleRequest :: State -> Request -> (Reply, State)
handleRequest state (Log { message = message }) =
  (ErrorReply { errorMessage = "hi" }, state)
handleRequest state GetState = (ReturnState { returnState = state }, state)

