{-# LANGUAGE DeriveGeneric #-}
module ClientStates.Lobby.Types where

import           ClientStates.Generic.Types
import           Data.Aeson.Types
import           GHC.Generics

data LobbyRequest = Unqueue
                    deriving (Generic, Show)

data LobbyResponse = StillQueued
                   | Unqueued
                     deriving (Generic, Show)


instance FromJSON LobbyRequest where
  parseJSON = genericParseJSON customOptions

instance ToJSON LobbyResponse where
  toEncoding = genericToEncoding customOptions
