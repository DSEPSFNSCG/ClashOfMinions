{-# LANGUAGE DeriveGeneric #-}
module ClientStates.Waiting.Types where


import           ClientStates.Generic.Types
import           Data.Aeson.Types
import           GHC.Generics

data RestoreGameRequest = RestoreGameRequest { gameID      :: Int
                                             , token       :: String
                                             , historyFrom :: Maybe Int }
                          deriving (Generic, Show)

data WaitingRequest = NewGame { name :: String }
                    | RestoreGame RestoreGameRequest
                    deriving (Generic, Show)

data RestoreGameResponse = RestoreSuccess
                       | InvalidGameID
                       | TokenMismatch
                       | GameOver
                       deriving (Generic, Show)


data WaitingResponse = Queued
                     | RestoreGameResponse RestoreGameResponse
                     deriving (Generic, Show)


instance FromJSON WaitingRequest where
  parseJSON = genericParseJSON customOptions

instance FromJSON RestoreGameRequest where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON WaitingResponse where
  toEncoding = genericToEncoding customOptions

instance ToJSON RestoreGameResponse where
  toEncoding = genericToEncoding defaultOptions
