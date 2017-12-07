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
data Placement = MkPlacement { f_position :: (Int, Int)
                             , f_stats    :: Stats
                             }
                 deriving (Generic, Show)

data GameResponse = PlaceSuccess
                  | InvalidPlacing String
                  | InvalidGameRequest
                  | LogResponse String
                  | NotYourTurn
                  | GameStart { f_gameId    :: Int
                              , f_youStart  :: Bool
                              , f_otherName :: String
                              , f_token     :: Token
                              , f_left      :: Bool}
                    deriving (Generic, Show)

data GameEvent = PlayerDisconnect
               | PlayerRequest GameRequest
               | PlayerReconnect



data Stats = MkStats { f_health         :: Int
                     , f_boosthealrange :: Int
                     , f_attackrange    :: Int
                     , f_attackdmg      :: Int
                     , f_healstrength   :: Int
                     }
             deriving (Generic, Show)


data Minion = MkMinion { minionId    :: Int
                       , minionStats :: Stats
                       , owner       :: Player
                       }
            deriving (Show)

instance Eq Minion where
  m == m' = (minionId m) == (minionId m')

type Field = Matrix (Maybe Minion)



data GameState = MkGameState { history       :: History
                             , field         :: Field
                             , currentPlayer :: Player
                             , waitingPlayer :: Player
                             , nextMinionId  :: Int
                             }
                 deriving (Show)

simpleShow :: GameState -> String
simpleShow g = "History: [...] (length " ++ show (length (history g)) ++ "), " ++
               "CurrentPlayer: " ++ show (currentPlayer g) ++ ", " ++
               "WaitingPlayer: " ++ show (waitingPlayer g) ++ ", " ++
               "NextMinionId: " ++ show (nextMinionId g) ++ ", " ++
               "Field:\n" ++ show (fmap minimaybe (field g)) where
    minimaybe :: Maybe Minion -> String
    minimaybe Nothing = "  "
    minimaybe (Just (MkMinion { minionId = i, owner = MkPlayer { leftPlayer = True } })) = show i ++ ">"
    minimaybe (Just (MkMinion { minionId = i, owner = MkPlayer { leftPlayer = False } })) = "<" ++ show i

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
                       , leftPlayer   :: Bool
                       }

instance Show Player where
  show = playerName

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
