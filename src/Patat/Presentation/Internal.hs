--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Presentation.Internal
    ( Presentation (..)
    , PresentationSettings (..)
    , defaultPresentationSettings
    , Slide (..)
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.TH.Extended as A
import           Data.Monoid            (Monoid)
import qualified Patat.Theme            as Theme
import           Prelude
import qualified Text.Pandoc            as Pandoc


--------------------------------------------------------------------------------
data Presentation = Presentation
    { pFilePath    :: !FilePath
    , pTitle       :: ![Pandoc.Inline]
    , pAuthor      :: ![Pandoc.Inline]
    , pSettings    :: !PresentationSettings
    , pSlides      :: [Slide]
    , pActiveSlide :: !Int
    } deriving (Show)


--------------------------------------------------------------------------------
-- | These are patat-specific settings.  That is where they differ from more
-- general metadata (author, title...)
data PresentationSettings = PresentationSettings
    { psTheme :: !(Maybe Theme.Theme)
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid PresentationSettings where
    mempty      = PresentationSettings Nothing
    mappend l r = PresentationSettings
        { psTheme = psTheme l `mappend` psTheme r
        }


--------------------------------------------------------------------------------
defaultPresentationSettings :: PresentationSettings
defaultPresentationSettings = PresentationSettings
    { psTheme = Just Theme.defaultTheme
    }


--------------------------------------------------------------------------------
newtype Slide = Slide {unSlide :: [Pandoc.Block]}
    deriving (Monoid, Show)


--------------------------------------------------------------------------------
$(A.deriveJSON A.dropPrefixOptions ''PresentationSettings)
