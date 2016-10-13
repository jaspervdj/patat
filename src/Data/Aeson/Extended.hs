{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Aeson.Extended
    ( module Data.Aeson

    , FlexibleNum (..)
    ) where

import           Control.Applicative ((<$>))
import           Data.Aeson
import qualified Data.Text           as T
import           Text.Read           (readMaybe)
import           Prelude

-- | This can be parsed from a JSON string in addition to a JSON number.
newtype FlexibleNum a = FlexibleNum {unFlexibleNum :: a}
    deriving (Show, ToJSON)

instance (FromJSON a, Read a) => FromJSON (FlexibleNum a) where
    parseJSON (String str) = case readMaybe (T.unpack str) of
        Nothing -> fail $ "Could not parse " ++ T.unpack str ++ " as a number"
        Just x  -> return (FlexibleNum x)
    parseJSON val = FlexibleNum <$> parseJSON val
