module Client where

import           Client.Types
import           ClientStates.Generic.Types
import           ClientStates.Lobby.Types
import           ClientStates.Waiting.Types

import qualified Control.Concurrent              as C
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.STM.Class
import           Control.Monad.Trans.Writer.Lazy (WriterT)
import           Data.Aeson
import           Data.ByteString.Char8           (ByteString, hGetLine,
                                                  hPutStrLn)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           System.IO                       (Handle, hClose, hIsEOF)
import           Types

instance Eq Client where
  c1 == c2 = (clientId c1) == (clientId c2)

instance Show Client where
  show c = show (clientId c)

clientReceive :: Client -> IO (Maybe ByteString)
clientReceive client = do
  isEOF <- hIsEOF $ clientHandle client
  if isEOF
    then return Nothing
    else do
      line <- hGetLine $ clientHandle client
      return $ Just line

instance ClientT Client where
  clientSend c = hPutStrLn (clientHandle c)
  setReceiver c q = writeTVar (clientQueue c) q
  close c = hClose (clientHandle c)

  clientRun client = do
    msg <- clientReceive client
    let result = case msg of
            Nothing  -> Disconnect
            Just str -> Input str
    atomically $ do
      queue <- readTVar $ clientQueue client
      writeTQueue queue $ (client, result)
    case msg of
      Nothing -> return ()
      Just _  -> clientRun client



parseBytes :: (FromJSON r) => ByteString -> Maybe (Request r)
parseBytes s = case decode $ fromStrict s :: Maybe GenericRequest of
  Just gr -> Just $ Left gr
  Nothing -> case decode $ fromStrict s of
    Just r  -> Just $ Right r
    Nothing -> Nothing

getEvent :: (ClientT c, FromJSON r) => ClientQueue c -> STM (c, Event (Maybe (Request r)))
getEvent queue = do
  (client, event) <- readTQueue queue
  return (client, fmap parseBytes event)

send :: (ClientT c, ToJSON resp) => c -> Response resp -> IO ()
send client (Left response)  = clientSend client $ toStrict $ encode response
send client (Right response) = clientSend client $ toStrict $ encode response


clog :: ClientT c => c -> String -> IO ()
clog client str = do
  putStrLn $ "Client " ++ show client ++ ": " ++ str



handleClient :: ClientQueue Client -> Handle -> Int -> IO ()
handleClient queue handle cid = do
  queueVar <- atomically $ newTVar queue
  let client = MkClient { clientId = cid
                        , clientHandle = handle
                        , clientQueue = queueVar
                        }
  atomically $ writeTQueue queue (client, Connect)
  clientRun client



