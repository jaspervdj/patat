--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Presentation.Internal
    ( Presentation (..)
    , PresentationSettings (..)
    , defaultPresentationSettings
    , Slide (..)
    , Fragment (..)
    , Index
    , getActiveFragment
    ) where


--------------------------------------------------------------------------------
import           Control.Monad          (mplus)
import qualified Data.Aeson.Extended    as A
import qualified Data.Aeson.TH.Extended as A
import           Data.Maybe             (listToMaybe)
import           Data.Monoid            (Monoid (..))
import qualified Patat.Theme            as Theme
import qualified Text.Pandoc            as Pandoc
import           Prelude


--------------------------------------------------------------------------------
data Presentation = Presentation
    { pFilePath       :: !FilePath
    , pTitle          :: ![Pandoc.Inline]
    , pAuthor         :: ![Pandoc.Inline]
    , pSettings       :: !PresentationSettings
    , pSlides         :: [Slide]
    , pActiveFragment :: !Index
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
newtype Slide = Slide {unSlide :: [Fragment]}
    deriving (Monoid, Show)


--------------------------------------------------------------------------------
newtype Fragment = Fragment {unFragment :: [Pandoc.Block]}
    deriving (Monoid, Show)


--------------------------------------------------------------------------------
-- | Active slide, active fragment.
type Index = (Int, Int)


--------------------------------------------------------------------------------
getActiveFragment :: Presentation -> Maybe Fragment
getActiveFragment presentation = do
    let (sidx, fidx) = pActiveFragment presentation
    Slide fragments <- listToMaybe $ drop sidx (pSlides presentation)
    listToMaybe $ drop fidx fragments


--------------------------------------------------------------------------------
$(A.deriveJSON A.dropPrefixOptions ''PresentationSettings)
