module Client ( handleClient
              , setReceiver
              , getRequest
              , sendResponse
              , close
              ) where

import           Client.Types
import           Types

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


handleClient :: ByteQueue -> Handle -> Int -> IO ()
handleClient queue handle cid = do
  queueVar <- atomically $ newTVar queue
  let client =  MkClient { clientId = cid
                       , clientHandle = handle
                       , clientQueue = queueVar
                       }
  sendResponse client $ WaitLogResponse { w_message = "Connected!" }
  clientRun client

clientReceive :: Client -> IO (Maybe ByteString)
clientReceive client = do
  isEOF <- hIsEOF $ clientHandle client
  if isEOF
    then return Nothing
    else do
      line <- hGetLine $ clientHandle client
      return $ Just line

clientRun client = do
  msg <- clientReceive client
  atomically $ do
    queue <- readTVar $ clientQueue client
    writeTQueue queue $ (client, msg)
  case msg of
    Nothing -> return ()
    Just _  -> clientRun client

setReceiver :: Client -> ByteQueue -> STM ()
setReceiver c q = writeTVar (clientQueue c) q

close :: Client -> IO ()
close c = hClose (clientHandle c)

getRequest :: FromJSON r => ByteQueue -> STM (ClientRequest r)
getRequest queue = do
  (client, event) <- readTQueue queue
  case event of
    Nothing -> return (client, Just Nothing)
    Just evt -> case decode $ fromStrict evt of
      Nothing  -> return (client, Nothing)
      Just req -> return (client, Just req)

sendResponse :: ToJSON r => Client -> r -> IO ()
sendResponse client response = hPutStrLn (clientHandle client) $ toStrict $ encode response


