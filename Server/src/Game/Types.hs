{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Game.Types where

import           Client.Types
import           Utils

import           Control.Concurrent.STM
import           Data.Aeson.Types
import           Data.Matrix
import           Data.Maybe
import           GHC.Generics
import           GHC.Word

subOptions = customOptions
  { tagSingleConstructors = False
  }


data GameRequest = Place Placement
                   deriving (Generic, Show)

type History = [Placement]
data Placement = Placement { f_position :: (Int, Int)
                           , f_stats    :: Stats
                           }
                 deriving (Generic, Show)

data GameResponse = PlaceSuccess
                  | InvalidPlacing
                  | InvalidGameRequest
                  | LogResponse String
                  | GameStart { f_gameId    :: Int
                              , f_youStart  :: Bool
                              , f_otherName :: String
                              , f_token     :: Token }
                    deriving (Generic, Show)

data GameEvent = PlayerDisconnect
               | PlayerRequest GameRequest
               | PlayerReconnect



data Stats = MkStats { f_health :: Int
                     }
             deriving (Generic, Show)


data Minion = MkMinion { f_minionStats :: Stats }
type Field = Matrix (Maybe Minion)



data GameState = MkGameState { history       :: History
                             , field         :: Field
                             , currentPlayer :: Player
                             , waitingPlayer :: Player
                             }

type GameQueue = TQueue (Player, GameEvent)

type GameID = Int
data Game = MkGame
  { gameId     :: Int
  , p1         :: Player
  , p2         :: Player
  , gameState  :: TVar GameState
  , transQueue :: ByteQueue
  , gameQueue  :: GameQueue }


type Token = Word64
data Player = MkPlayer { playerClient :: TVar (Maybe Client)
                       , playerName   :: String
                       , playerToken  :: Token
                       }

instance Eq Player where
  p == p' = (playerToken p) == (playerToken p')

instance FromJSON Stats where
  parseJSON = genericParseJSON subOptions
instance FromJSON GameRequest where
  parseJSON = genericParseJSON customOptions
instance FromJSON Placement where
  parseJSON = genericParseJSON subOptions

instance ToJSON Placement where
  toEncoding = genericToEncoding subOptions
instance ToJSON Stats where
  toEncoding = genericToEncoding subOptions
instance ToJSON GameRequest where
  toEncoding = genericToEncoding customOptions
instance ToJSON GameResponse where
  toEncoding = genericToEncoding customOptions
