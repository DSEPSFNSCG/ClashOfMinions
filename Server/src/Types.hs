{-# LANGUAGE DeriveGeneric #-}
module Types where

import           Client.Types
import           Control.Concurrent.STM          (STM, TVar)
import           Control.Monad.Trans.Writer.Lazy (WriterT)
import           Data.Aeson
import           Data.Aeson.Types
import           Game.Types
import           GHC.Generics
import           System.Random
import           Utils

data Server = Server { initQueue      :: ByteQueue
                     , waitQueue      :: ByteQueue
                     , serverStateVar :: TVar ServerState
                     }

data ServerState = ServerState { games   :: [Game]
                               , inQueue :: Maybe (Client, String)
                               , rng     :: StdGen
                               }

initialServerState :: IO ServerState
initialServerState = do
  rng <- getStdGen
  return $ ServerState { games = []
                       , inQueue = Nothing
                       , rng = rng
                       }


data RestoreGameRequest = RestoreGameRequest { g_gameId      :: Int
                                             , g_token       :: Token
                                             , g_historyFrom :: Maybe Int }
                          deriving (Generic, Show)

data WaitingRequest = NewGame { name :: String }
                    | RestoreGame RestoreGameRequest
                    deriving (Generic, Show)

type LogSTM a = WriterT String STM a

type TransResult r a = LogSTM (ServerState, Maybe (Either r a))

data WaitingResponse = Queued
                     | RestoreSuccess History
                     | InvalidGameID
                     | TokenMismatch
                     | GameOver
                     | InvalidWaitingRequest { validWaitingRequests :: [WaitingRequest] }
                     | WaitLogResponse { w_message :: String }
                     deriving (Generic, Show)

invalidWaitingRequest = InvalidWaitingRequest { validWaitingRequests =
                                                [ NewGame { name = "Foobar" }
                                                , RestoreGame $ RestoreGameRequest
                                                  { g_gameId = 3
                                                  , g_token = "450472606304"
                                                  , g_historyFrom = Just 5 }
                                                ]
                                              }


instance FromJSON WaitingRequest where
  parseJSON = genericParseJSON customOptions
instance ToJSON WaitingRequest where
  toEncoding = genericToEncoding customOptions

instance FromJSON RestoreGameRequest where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = stripPrefixes }
instance ToJSON RestoreGameRequest where
  toEncoding = genericToEncoding $ defaultOptions { fieldLabelModifier = stripPrefixes }

instance ToJSON WaitingResponse where
  toEncoding = genericToEncoding customOptions

