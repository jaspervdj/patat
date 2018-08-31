--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Presentation.Internal
    ( Presentation (..)
    , PresentationSettings (..)
    , defaultPresentationSettings

    , Margins (..)
    , marginsOf

    , ExtensionList (..)
    , defaultExtensionList

    , ImageSettings (..)

    , Slide (..)
    , Fragment (..)
    , Index

    , getSlide
    , numFragments

    , ActiveFragment (..)
    , getActiveFragment
    ) where


--------------------------------------------------------------------------------
import           Control.Monad          (mplus)
import qualified Data.Aeson.Extended    as A
import qualified Data.Aeson.TH.Extended as A
import qualified Data.Foldable          as Foldable
import           Data.List              (intercalate)
import           Data.Maybe             (fromMaybe, listToMaybe)
import           Data.Monoid            (Monoid (..))
import           Data.Semigroup         (Semigroup (..))
import qualified Data.Text              as T
import qualified Patat.Theme            as Theme
import           Prelude
import qualified Text.Pandoc            as Pandoc
import           Text.Read              (readMaybe)


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
    { psRows             :: !(Maybe (A.FlexibleNum Int))
    , psColumns          :: !(Maybe (A.FlexibleNum Int))
    , psMargins          :: !(Maybe Margins)
    , psWrap             :: !(Maybe Bool)
    , psTheme            :: !(Maybe Theme.Theme)
    , psIncrementalLists :: !(Maybe Bool)
    , psAutoAdvanceDelay :: !(Maybe (A.FlexibleNum Int))
    , psSlideLevel       :: !(Maybe Int)
    , psPandocExtensions :: !(Maybe ExtensionList)
    , psImages           :: !(Maybe ImageSettings)
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup PresentationSettings where
    l <> r = PresentationSettings
        { psRows             = psRows             l `mplus` psRows             r
        , psColumns          = psColumns          l `mplus` psColumns          r
        , psMargins          = psMargins          l <>      psMargins          r
        , psWrap             = psWrap             l `mplus` psWrap             r
        , psTheme            = psTheme            l <>      psTheme            r
        , psIncrementalLists = psIncrementalLists l `mplus` psIncrementalLists r
        , psAutoAdvanceDelay = psAutoAdvanceDelay l `mplus` psAutoAdvanceDelay r
        , psSlideLevel       = psSlideLevel       l `mplus` psSlideLevel       r
        , psPandocExtensions = psPandocExtensions l `mplus` psPandocExtensions r
        , psImages           = psImages           l `mplus` psImages           r
        }


--------------------------------------------------------------------------------
instance Monoid PresentationSettings where
    mappend = (<>)
    mempty  = PresentationSettings
                    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                    Nothing Nothing Nothing


--------------------------------------------------------------------------------
defaultPresentationSettings :: PresentationSettings
defaultPresentationSettings = PresentationSettings
    { psRows             = Nothing
    , psColumns          = Nothing
    , psMargins          = Just defaultMargins
    , psWrap             = Nothing
    , psTheme            = Just Theme.defaultTheme
    , psIncrementalLists = Nothing
    , psAutoAdvanceDelay = Nothing
    , psSlideLevel       = Nothing
    , psPandocExtensions = Nothing
    , psImages           = Nothing
    }


--------------------------------------------------------------------------------
data Margins = Margins
    { mLeft  :: !(Maybe (A.FlexibleNum Int))
    , mRight :: !(Maybe (A.FlexibleNum Int))
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup Margins where
    l <> r = Margins
        { mLeft  = mLeft  l `mplus` mLeft  r
        , mRight = mRight l `mplus` mRight r
        }


--------------------------------------------------------------------------------
instance Monoid Margins where
    mappend = (<>)
    mempty  = Margins Nothing Nothing


--------------------------------------------------------------------------------
defaultMargins :: Margins
defaultMargins = Margins
    { mLeft  = Nothing
    , mRight = Nothing
    }


--------------------------------------------------------------------------------
marginsOf :: PresentationSettings -> (Int, Int)
marginsOf presentationSettings =
    (marginLeft, marginRight)
  where
    margins    = fromMaybe defaultMargins $ psMargins presentationSettings
    marginLeft  = fromMaybe 0 (A.unFlexibleNum <$> mLeft margins)
    marginRight = fromMaybe 0 (A.unFlexibleNum <$> mRight margins)


--------------------------------------------------------------------------------
newtype ExtensionList = ExtensionList {unExtensionList :: Pandoc.Extensions}
    deriving (Show)


--------------------------------------------------------------------------------
instance A.FromJSON ExtensionList where
    parseJSON = A.withArray "FromJSON ExtensionList" $
        fmap (ExtensionList . mconcat) . mapM parseExt . Foldable.toList
      where
        parseExt = A.withText "FromJSON ExtensionList" $ \txt -> case txt of
            -- Our default extensions
            "patat_extensions" -> return (unExtensionList defaultExtensionList)

            -- Individuals
            _ -> case readMaybe ("Ext_" ++ T.unpack txt) of
                Just e  -> return $ Pandoc.extensionsFromList [e]
                Nothing -> fail $
                    "Unknown extension: " ++ show txt ++
                    ", known extensions are: " ++
                    intercalate ", "
                        [ show (drop 4 (show e))
                        | e <- [minBound .. maxBound] :: [Pandoc.Extension]
                        ]


--------------------------------------------------------------------------------
defaultExtensionList :: ExtensionList
defaultExtensionList = ExtensionList $
    Pandoc.readerExtensions Pandoc.def `mappend` Pandoc.extensionsFromList
    [ Pandoc.Ext_yaml_metadata_block
    , Pandoc.Ext_table_captions
    , Pandoc.Ext_simple_tables
    , Pandoc.Ext_multiline_tables
    , Pandoc.Ext_grid_tables
    , Pandoc.Ext_pipe_tables
    , Pandoc.Ext_raw_html
    , Pandoc.Ext_tex_math_dollars
    , Pandoc.Ext_fenced_code_blocks
    , Pandoc.Ext_fenced_code_attributes
    , Pandoc.Ext_backtick_code_blocks
    , Pandoc.Ext_inline_code_attributes
    , Pandoc.Ext_fancy_lists
    , Pandoc.Ext_four_space_rule
    , Pandoc.Ext_definition_lists
    , Pandoc.Ext_compact_definition_lists
    , Pandoc.Ext_example_lists
    , Pandoc.Ext_strikeout
    , Pandoc.Ext_superscript
    , Pandoc.Ext_subscript
    ]


--------------------------------------------------------------------------------
data ImageSettings = ImageSettings
    { isBackend :: !T.Text
    , isParams  :: !A.Object
    } deriving (Show)


--------------------------------------------------------------------------------
instance A.FromJSON ImageSettings where
    parseJSON = A.withObject "FromJSON ImageSettings" $ \o -> do
        t <- o A..: "backend"
        return ImageSettings {isBackend = t, isParams = o}


--------------------------------------------------------------------------------
data Slide
    = ContentSlide [Fragment]
    | TitleSlide   Pandoc.Block
    deriving (Show)


--------------------------------------------------------------------------------
newtype Fragment = Fragment {unFragment :: [Pandoc.Block]}
    deriving (Monoid, Semigroup, Show)


--------------------------------------------------------------------------------
-- | Active slide, active fragment.
type Index = (Int, Int)


--------------------------------------------------------------------------------
getSlide :: Int -> Presentation -> Maybe Slide
getSlide sidx = listToMaybe . drop sidx . pSlides


--------------------------------------------------------------------------------
numFragments :: Slide -> Int
numFragments (ContentSlide fragments) = length fragments
numFragments (TitleSlide _)           = 1


--------------------------------------------------------------------------------
data ActiveFragment = ActiveContent Fragment | ActiveTitle Pandoc.Block
    deriving (Show)


--------------------------------------------------------------------------------
getActiveFragment :: Presentation -> Maybe ActiveFragment
getActiveFragment presentation = do
    let (sidx, fidx) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    case slide of
        TitleSlide   block     -> return (ActiveTitle block)
        ContentSlide fragments ->
            fmap ActiveContent . listToMaybe $ drop fidx fragments


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''PresentationSettings)
$(A.deriveFromJSON A.dropPrefixOptions ''Margins)
