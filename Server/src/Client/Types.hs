module Client.Types where

import           Control.Concurrent.STM (TQueue, TVar)
import           Data.ByteString.Char8  (ByteString)
import           System.IO              (Handle)

type ByteQueue = TQueue (Client, Maybe ByteString)

type ClientRequest r = (Client, Maybe (Maybe r))

data Client =
  MkClient { clientId     :: Int
           , clientHandle :: Handle
           , clientQueue  :: TVar ByteQueue
           }

instance Eq Client where
  c1 == c2 = (clientId c1) == (clientId c2)

instance Show Client where
  show c = show (clientId c)


