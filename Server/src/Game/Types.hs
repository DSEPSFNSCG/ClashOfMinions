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
                  | OtherPlayerPlaced Placement
                  | InvalidPlacing String
                  | InvalidGameRequest { validGameRequests :: [GameRequest] }
                  | GameLogResponse String
                  | NotYourTurn
                  | GameOver { f_won :: Bool}
                  | GameStart { f_gameId    :: Int
                              , f_youStart  :: Bool
                              , f_otherName :: String
                              , f_token     :: Token
                              }
                    deriving (Generic, Show)

invalidGameRequest = InvalidGameRequest
  { validGameRequests =
    [ Place $ MkPlacement
      { f_position = (0, 3)
      , f_stats = MkStats { f_attackdmg = 0
                          , f_attackrange = 0
                          , f_buffrange = 0
                          , f_healing = 0
                          , f_atkbuff = 0
                          , f_healbuff = 0
                          , f_shield = 0
                          , f_maxhealth = 0 }
      }
    ]
  }


data GameEvent = PlayerDisconnect
               | PlayerRequest GameRequest
               | PlayerReconnect



data Stats = MkStats { f_attackdmg   :: Int
                     , f_attackrange :: Int
                     , f_buffrange   :: Int
                     , f_healing     :: Int
                     , f_atkbuff     :: Int
                     , f_healbuff    :: Int
                     , f_shield      :: Int
                     , f_maxhealth   :: Int
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
               | GameDone { winningPlayer :: Player
                          , losingPlayer  :: Player }
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


type Token = String
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
