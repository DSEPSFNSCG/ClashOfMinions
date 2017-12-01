{-# LANGUAGE DeriveGeneric #-}
module Game.Types where

import           Client.Types
import           ClientStates.Generic.Types
import           Data.Aeson.Types
import           Data.Word
import           GHC.Generics

data GameRequest = Place
                   deriving (Generic, Show)

data GameResponse = Success
                  | InvalidPlacing
                  | Placed
                    deriving (Generic, Show)


instance FromJSON GameRequest where
  parseJSON = genericParseJSON customOptions

instance ToJSON GameResponse where
  toEncoding = genericToEncoding customOptions


type GameID = Int
data Game c = MkGame
  { gameId        :: GameID
  , currentPlayer :: Player c
  , waitingPlayer :: Player c
  , gameState     :: ()
  , gameQueue     :: ClientQueue c}


data Move = Move { test :: String
                 }

type PlayerState = ()

data Player c = MkPlayer { playerClient :: c
                         , playerState  :: PlayerState
                         , playerName   :: String
                         , playerToken  :: Word64
                         } deriving (Show)
