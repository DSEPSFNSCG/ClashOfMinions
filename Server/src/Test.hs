module Test where

import           Client.Types
import           Control.Concurrent
import           Data.Aeson
import           Data.ByteString.Char8 (ByteString, hPutStrLn)
import           Data.ByteString.Lazy  (toStrict)
import           Game.Types
import qualified Network               as N
import           Network.Simple.TCP
import qualified Network.Socket        as S
import           Pair.Types
import           Server
import           System.IO             (BufferMode (LineBuffering), Handle,
                                        IOMode (ReadWriteMode), hGetLine,
                                        hSetBuffering)
import           Types

defaultPort :: N.PortID
defaultPort = N.PortNumber 8081


{--
How to test:
- Run "stack ghci"
- Enter "(a, b) <- gameTest". This starts a server and 2 clients, a and b can be used to send requests from the clients:
- Send a placement: "a $ Place $ Placement { f_position = (0,0), f_stats = MkStats { f_health = 10 } }"

All the games server messages are prefixed with [GAME]
All the clients received responses are prefixed with [Client1]/[Client2]
--}

gameTest :: IO (GameRequest -> IO (), GameRequest -> IO ())
gameTest = do
  forkIO $ server defaultPort
  threadDelay 10
  (w, l, g) <- runClient "[Client1]"
  (w', l', g') <- runClient "[Client2]"
  w $ NewGame { name = "Client1" }
  w' $ NewGame { name = "Client2" }
  return (g, g')


runClient :: String -> IO (WaitingRequest -> IO (), LobbyRequest -> IO (), GameRequest -> IO ())
runClient prefix = do
  connect "localhost" "8081" $ \(socket, _) -> do
    handle <- S.socketToHandle socket ReadWriteMode
    hSetBuffering handle LineBuffering
    forkIO $ toStdout prefix handle
    return (sendClient handle, sendClient handle, sendClient handle)

toStdout :: String -> Handle -> IO ()
toStdout prefix handle = do
  line <- hGetLine handle
  putStrLn $ prefix ++ line
  toStdout prefix handle

sendClient :: ToJSON r => Handle -> r -> IO ()
sendClient handle r = hPutStrLn handle $ toStrict $ encode r


