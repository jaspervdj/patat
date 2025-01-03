{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Aeson.Extended
    ( module Data.Aeson

    , FlexibleNum (..)

    , resultToEither
    ) where

import           Data.Aeson
import qualified Data.Text        as T
import           Prelude
import           Text.Read        (readMaybe)

-- | This can be parsed from a JSON string in addition to a JSON number.
newtype FlexibleNum a = FlexibleNum {unFlexibleNum :: a}
    deriving (Eq, Show, ToJSON)

instance (FromJSON a, Read a) => FromJSON (FlexibleNum a) where
    parseJSON (String str) = case readMaybe (T.unpack str) of
        Nothing -> fail $ "Could not parse " ++ T.unpack str ++ " as a number"
        Just x  -> return (FlexibleNum x)
    parseJSON val = FlexibleNum <$> parseJSON val

resultToEither :: Result a -> Either String a
resultToEither (Success x)   = Right x
resultToEither (Error   err) = Left err
