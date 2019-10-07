--------------------------------------------------------------------------------
module Data.Aeson.TH.Extended
    ( module Data.Aeson.TH
    , dropPrefixOptions
    ) where


--------------------------------------------------------------------------------
import           Data.Aeson.TH
import           Data.Char     (isUpper, toLower)


--------------------------------------------------------------------------------
dropPrefixOptions :: Options
dropPrefixOptions = defaultOptions
    { fieldLabelModifier = dropPrefix
    }
  where
    dropPrefix str = case break isUpper str of
        (_, (y : ys)) -> toLower y : ys
        _             -> str
