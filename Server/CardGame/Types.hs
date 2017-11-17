{-# LANGUAGE DeriveGeneric #-}
module CardGame.Types where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString      as BS
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           GHC.Generics
import qualified Network.Socket       as NS
import           System.IO            (Handle)

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
