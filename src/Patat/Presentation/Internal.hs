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
import           Control.Monad          (mplus)
import qualified Data.Aeson.Extended    as A
import qualified Data.Aeson.TH.Extended as A
import           Data.Monoid            (Monoid (..))
import qualified Patat.Theme            as Theme
import qualified Text.Pandoc            as Pandoc
import           Prelude


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
    { psRows    :: !(Maybe (A.FlexibleNum Int))
    , psColumns :: !(Maybe (A.FlexibleNum Int))
    , psWrap    :: !(Maybe Bool)
    , psTheme   :: !(Maybe Theme.Theme)
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid PresentationSettings where
    mempty      = PresentationSettings Nothing Nothing Nothing Nothing
    mappend l r = PresentationSettings
        { psRows    = psRows    l `mplus`   psRows    r
        , psColumns = psColumns l `mplus`   psColumns r
        , psWrap    = psWrap    l `mplus`   psWrap    r
        , psTheme   = psTheme   l `mappend` psTheme   r
        }


--------------------------------------------------------------------------------
defaultPresentationSettings :: PresentationSettings
defaultPresentationSettings = PresentationSettings
    { psRows    = Nothing
    , psColumns = Nothing
    , psWrap    = Nothing
    , psTheme   = Just Theme.defaultTheme
    }


--------------------------------------------------------------------------------
newtype Slide = Slide {unSlide :: [Pandoc.Block]}
    deriving (Monoid, Show)


--------------------------------------------------------------------------------
$(A.deriveJSON A.dropPrefixOptions ''PresentationSettings)
