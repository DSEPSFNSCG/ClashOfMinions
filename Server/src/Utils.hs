module Utils ( customOptions
             , stripPrefixes
             ) where

import           Data.Aeson.Types
import           Data.Char
import           Data.List
import           Data.Maybe

lowerFirst :: String -> String
lowerFirst (x:xs) = toLower x : xs

stripPrefixes :: String -> String
stripPrefixes ('g':'_':xs) = xs
stripPrefixes (x:'_':xs)   = xs
stripPrefixes xs           = xs

customOptions :: Options
customOptions = defaultOptions {
  sumEncoding = defaultTaggedObject {
    tagFieldName = "type"
  },
  tagSingleConstructors = True,
  allNullaryToStringTag = False,
  constructorTagModifier = lowerFirst,
  fieldLabelModifier = stripPrefixes
}
