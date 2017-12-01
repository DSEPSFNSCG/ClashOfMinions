module Client.Types where

import           ClientStates.Generic.Types
import           Control.Concurrent.STM          (STM, TQueue, TVar, writeTVar)
import           Control.Monad.Trans.Writer.Lazy (WriterT)
import           Data.ByteString.Char8           (ByteString, hPutStrLn)
import           System.IO                       (Handle, hClose)

data Event r = Connect
           | Input r
           | Disconnect

instance Functor Event where
  fmap f e = case e of
    Connect    -> Connect
    Input r    -> Input $ f r
    Disconnect -> Disconnect

class (Eq c, Show c) => ClientT c where
  clientRun :: c -> IO ()
  clientSend :: c -> ByteString -> IO ()
  setReceiver :: c -> ClientQueue c -> STM ()
  close :: c -> IO ()

type ClientQueue c = TQueue (c, Event ByteString)



data Client =
  MkClient { clientId     :: Int
           , clientHandle :: Handle
           , clientQueue  :: TVar (ClientQueue Client)
           }

type ClientMonad a = WriterT String STM a

