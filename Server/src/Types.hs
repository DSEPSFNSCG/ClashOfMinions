module Types where

import           Client.Types
import           ClientStates.Generic.Types
import           ClientStates.Waiting.Types
import           Game.Types

import qualified Control.Concurrent              as C
import           Control.Concurrent.STM
import           Control.Monad.Trans.Writer.Lazy (WriterT)
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Char8           as BS
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           GHC.Generics
import qualified Network.Socket                  as NS
import           System.IO                       (Handle, hIsEOF)




data ServerState c = MkServerState { games :: [Game c]
                                 , inQueue :: Maybe (c, String)
                                 }

type WriterSTM a = WriterT String STM a




