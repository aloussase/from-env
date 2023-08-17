{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
module System.Environment.FromEnv.TryParse
(
    TryParse (..)
)
where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

-- | Class for things that may be parsed from strings.
class TryParse a where
  tryParse :: String -> Maybe a

instance TryParse Int where tryParse = readMaybe
instance TryParse Double where tryParse = readMaybe
instance TryParse Float where tryParse = readMaybe

instance TryParse [Char] where
   tryParse = Just

instance TryParse Char where
   tryParse [c] = Just c
   tryParse _   = Nothing

instance TryParse Text where
  tryParse = Just . T.pack

instance TryParse Bool where
  tryParse "" = Just False
  tryParse _  = Just True
