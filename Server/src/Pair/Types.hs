{-# LANGUAGE DeriveGeneric #-}
module Pair.Types where

import           Data.Aeson.Types
import           GHC.Generics
import           Utils

data LobbyRequest = Unqueue
                    deriving (Generic, Show)

data LobbyResponse = Unqueued
                   | InvalidPairingRequest
                     deriving (Generic, Show)


instance FromJSON LobbyRequest where
  parseJSON = genericParseJSON customOptions
instance ToJSON LobbyRequest where
  toEncoding = genericToEncoding customOptions

instance ToJSON LobbyResponse where
  toEncoding = genericToEncoding customOptions
