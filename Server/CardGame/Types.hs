{-# LANGUAGE DeriveGeneric #-}
module CardGame.Types where

import qualified Control.Concurrent    as C
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy  (fromStrict, toStrict)
import           GHC.Generics
import qualified Network.Socket        as NS
import           System.IO             (Handle, hIsEOF)

type State = Int

customOptions :: Options
customOptions = defaultOptions {
  sumEncoding = defaultTaggedObject {
    tagFieldName = "type"
  }
}

data Client = MkClient { clientId      :: Int
                       , clientHandle  :: Handle
                       , clientAddress :: NS.SockAddr }
                       deriving (Show)

clientSend :: Client -> Reply -> IO ()
clientSend client reply = BS.hPutStrLn (clientHandle client) $ toStrict $ encode reply

clientRecv :: Client -> IO (Maybe Request)
clientRecv client = do
  line <- BS.hGetLine (clientHandle client)
  return $ decode $ fromStrict line

data ClientMessage = Connect | RequestT Request | Disconnect deriving (Show)
type ClientChannel = C.Chan (Client, ClientMessage)

-- Puts all data from this client to the given channel
readClient :: Client -> C.Chan (Client, ClientMessage) -> IO ()
readClient client chan = do
  isEOF <- hIsEOF (clientHandle client)
  if isEOF
    then do
      putStrLn $ "Client disconnected: " ++ show client
      C.writeChan chan (client, Disconnect)
    else do
      line <- BS.hGetLine (clientHandle client)
      case decode (fromStrict line) of
        Nothing -> clientSend client $ ErrorReply { errorMessage = "Invalid request" }
        Just request -> C.writeChan chan (client, RequestT request)
      readClient client chan

instance Eq Client where
  c1 == c2 = (clientId c1) == (clientId c2)

type GameID = Int

data Request =
  Test {
    hello :: String
  }
  | Place
  | GetState
  deriving (Generic, Show)


data Reply =
  ErrorReply {
    errorMessage :: String
  }
  | ReturnState {
    returnState :: State
  }
  | Log {
    message :: String
  }
  deriving (Generic, Show)


instance ToJSON Request where
  toEncoding = genericToEncoding customOptions
instance FromJSON Request where
  parseJSON = genericParseJSON customOptions
instance ToJSON Reply where
  toEncoding = genericToEncoding customOptions
instance FromJSON Reply where
  parseJSON = genericParseJSON customOptions
