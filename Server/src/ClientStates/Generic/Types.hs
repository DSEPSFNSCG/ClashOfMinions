{-# LANGUAGE DeriveGeneric #-}
module ClientStates.Generic.Types where

import           Control.Concurrent.STM          (STM, TQueue, TVar, writeTVar)
import           Control.Monad.Trans.Writer.Lazy (WriterT)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Char8           (ByteString, hPutStrLn)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Data.Char
import           GHC.Generics
import           System.IO                       (Handle)

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs

customOptions :: Options
customOptions = defaultOptions {
  sumEncoding = defaultTaggedObject {
    tagFieldName = "type"
  },
  tagSingleConstructors = True,
  allNullaryToStringTag = False,
  constructorTagModifier = lowerFirst
}

data GenericRequest = GetState
                    | LogRequest { requestString :: String
                                 }
                    deriving (Generic, Show)

data GenericResponse = State { responseState :: String
                             }
                     | LogResponse { responseString :: String
                                   }
                     deriving (Generic, Show)

type Request r = Either GenericRequest r
type Response r = Either GenericResponse r


instance FromJSON GenericRequest where
  parseJSON = genericParseJSON customOptions

instance ToJSON GenericResponse where
  toEncoding = genericToEncoding customOptions

