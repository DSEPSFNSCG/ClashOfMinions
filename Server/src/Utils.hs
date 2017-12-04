module Utils ( customOptions
             ) where

import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs

customOptions :: Options
customOptions = defaultOptions {
  sumEncoding = defaultTaggedObject {
    tagFieldName = "type"
  },
  tagSingleConstructors = True,
  allNullaryToStringTag = False,
  constructorTagModifier = lowerFirst,
  fieldLabelModifier = (\f -> fromMaybe f (stripPrefix "f_" f))
}
