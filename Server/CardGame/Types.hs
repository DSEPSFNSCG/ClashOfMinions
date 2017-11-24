{-# LANGUAGE DeriveGeneric #-}
module CardGame.Types where

import qualified Control.Concurrent     as C
import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           GHC.Generics
import qualified Network.Socket         as NS
import           System.IO              (Handle, hIsEOF)


data Game = MkGame
  { gameId    :: GameID
  , players   :: (Player, Player)
  , gamestate :: ()
  , events    :: TQueue (Client, ClientMessage)}

data PlayerState = Waiting deriving (Show)

data Player = MkPlayer {
  playerHandle :: Handle,
  playerState  :: PlayerState
} deriving (Show)

data ServerState = MkServerState { games   :: [Game]
                                 , inQueue :: Maybe Client
                                 }

data ClientState = Unknown
                 | WaitingNewGame
                 | InGame Game

data Client = MkClient { clientId     :: Int
                       , clientHandle :: Handle
                       , clientState  :: TVar ClientState
                       }

instance Eq Client where
  c1 == c2 = (clientId c1) == (clientId c2)

data ClientMessage = RequestT Request | Disconnect

type State = Int

class HasHandle a where
  handle :: a -> Handle

instance HasHandle Client where
  handle = clientHandle
instance HasHandle Player where
  handle = playerHandle

tell :: HasHandle a => a -> Reply -> IO ()
tell a reply = BS.hPutStrLn (handle a) $ toStrict $ encode reply


type ClientChannel = C.Chan (Client, ClientMessage)

type GameID = Int

data Request = NewGame | Chat { chatMessage :: String }
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

customOptions :: Options
customOptions = defaultOptions {
  sumEncoding = defaultTaggedObject {
    tagFieldName = "type"
  }
}

instance ToJSON Request where
  toEncoding = genericToEncoding customOptions
instance FromJSON Request where
  parseJSON = genericParseJSON customOptions
instance ToJSON Reply where
  toEncoding = genericToEncoding customOptions
instance FromJSON Reply where
  parseJSON = genericParseJSON customOptions
