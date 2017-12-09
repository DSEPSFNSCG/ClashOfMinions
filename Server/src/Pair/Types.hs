{-# LANGUAGE DeriveGeneric #-}
module Pair.Types where

import           Data.Aeson.Types
import           GHC.Generics
import           Utils

data LobbyRequest = Unqueue
                    deriving (Generic, Show)

data LobbyResponse = Unqueued
                   | InvalidPairingRequest { validPairingRequests :: [LobbyRequest] }
                     deriving (Generic, Show)

invalidPairingRequest = invalidPairingRequest { validPairingRequests =
                                                [ Unqueue ] }

instance FromJSON LobbyRequest where
  parseJSON = genericParseJSON customOptions
instance ToJSON LobbyRequest where
  toEncoding = genericToEncoding customOptions

instance ToJSON LobbyResponse where
  toEncoding = genericToEncoding customOptions
