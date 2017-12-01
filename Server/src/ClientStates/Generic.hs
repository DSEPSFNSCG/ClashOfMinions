module ClientStates.Generic where

import           ClientStates.Generic.Types
import           Control.Concurrent.STM     (STM, readTQueue)
import           Data.Aeson                 (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Char8      (ByteString, hPutStrLn)
import           Data.ByteString.Lazy.Char8 (fromStrict, toStrict)

decodeRequest :: FromJSON r => ByteString -> Maybe (Request r)
decodeRequest str =
  case decode $ fromStrict str of
    Just generic -> Just $ Left generic
    Nothing -> case decode $ fromStrict str of
      Just req -> Just $ Right req
      Nothing  -> Nothing

encodeResponse :: ToJSON r => Response r -> ByteString
encodeResponse resp = toStrict $ encode resp
