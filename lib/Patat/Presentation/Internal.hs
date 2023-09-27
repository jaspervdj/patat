--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Presentation.Internal
    ( Breadcrumbs
    , Presentation (..)
    , PresentationSettings (..)
    , defaultPresentationSettings

    , MarginSettings (..)
    , Margins (..)
    , margins

    , ExtensionList (..)
    , defaultExtensionList

    , ImageSettings (..)

    , EvalSettingsMap
    , EvalSettings (..)

    , Slide (..)
    , SlideContent (..)
    , Instruction.Fragment (..)
    , Index

    , getSlide
    , numFragments

    , ActiveFragment (..)
    , activeFragment
    , activeSpeakerNotes
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                   (mplus)
import qualified Data.Aeson.Extended             as A
import qualified Data.Aeson.TH.Extended          as A
import qualified Data.Foldable                   as Foldable
import           Data.Function                   (on)
import qualified Data.HashMap.Strict             as HMS
import           Data.List                       (intercalate)
import           Data.Maybe                      (fromMaybe)
import           Data.Sequence.Extended          (Seq)
import qualified Data.Sequence.Extended          as Seq
import qualified Data.Text                       as T
import           Patat.EncodingFallback          (EncodingFallback)
import qualified Patat.Presentation.Instruction  as Instruction
import qualified Patat.Presentation.SpeakerNotes as SpeakerNotes
import qualified Patat.Theme                     as Theme
import           Prelude
import qualified Skylighting                     as Skylighting
import qualified Text.Pandoc                     as Pandoc
import           Text.Read                       (readMaybe)


--------------------------------------------------------------------------------
type Breadcrumbs = [(Int, [Pandoc.Inline])]


--------------------------------------------------------------------------------
data Presentation = Presentation
    { pFilePath         :: !FilePath
    , pEncodingFallback :: !EncodingFallback
    , pTitle            :: ![Pandoc.Inline]
    , pAuthor           :: ![Pandoc.Inline]
    , pSettings         :: !PresentationSettings
    , pSlides           :: !(Seq Slide)
    , pBreadcrumbs      :: !(Seq Breadcrumbs)  -- One for each slide.
    , pActiveFragment   :: !Index
    , pSyntaxMap        :: !Skylighting.SyntaxMap
    } deriving (Show)


--------------------------------------------------------------------------------
-- | These are patat-specific settings.  That is where they differ from more
-- general metadata (author, title...)
data PresentationSettings = PresentationSettings
    { psRows              :: !(Maybe (A.FlexibleNum Int))
    , psColumns           :: !(Maybe (A.FlexibleNum Int))
    , psMargins           :: !(Maybe MarginSettings)
    , psWrap              :: !(Maybe Bool)
    , psTheme             :: !(Maybe Theme.Theme)
    , psIncrementalLists  :: !(Maybe Bool)
    , psAutoAdvanceDelay  :: !(Maybe (A.FlexibleNum Int))
    , psSlideLevel        :: !(Maybe Int)
    , psPandocExtensions  :: !(Maybe ExtensionList)
    , psImages            :: !(Maybe ImageSettings)
    , psBreadcrumbs       :: !(Maybe Bool)
    , psEval              :: !(Maybe EvalSettingsMap)
    , psSlideNumber       :: !(Maybe Bool)
    , psSyntaxDefinitions :: !(Maybe [FilePath])
    , psSpeakerNotes      :: !(Maybe SpeakerNotes.Settings)
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup PresentationSettings where
    l <> r = PresentationSettings
        { psRows              = on mplus psRows              l r
        , psColumns           = on mplus psColumns           l r
        , psMargins           = on (<>)  psMargins           l r
        , psWrap              = on mplus psWrap              l r
        , psTheme             = on (<>)  psTheme             l r
        , psIncrementalLists  = on mplus psIncrementalLists  l r
        , psAutoAdvanceDelay  = on mplus psAutoAdvanceDelay  l r
        , psSlideLevel        = on mplus psSlideLevel        l r
        , psPandocExtensions  = on mplus psPandocExtensions  l r
        , psImages            = on mplus psImages            l r
        , psBreadcrumbs       = on mplus psBreadcrumbs       l r
        , psEval              = on (<>)  psEval              l r
        , psSlideNumber       = on mplus psSlideNumber       l r
        , psSyntaxDefinitions = on (<>)  psSyntaxDefinitions l r
        , psSpeakerNotes      = on mplus psSpeakerNotes      l r
        }


--------------------------------------------------------------------------------
instance Monoid PresentationSettings where
    mappend = (<>)
    mempty  = PresentationSettings
                    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                    Nothing


--------------------------------------------------------------------------------
defaultPresentationSettings :: PresentationSettings
defaultPresentationSettings = mempty
    { psMargins          = Just defaultMargins
    , psTheme            = Just Theme.defaultTheme
    }


--------------------------------------------------------------------------------
data MarginSettings = MarginSettings
    { msTop   :: !(Maybe (A.FlexibleNum Int))
    , msLeft  :: !(Maybe (A.FlexibleNum Int))
    , msRight :: !(Maybe (A.FlexibleNum Int))
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup MarginSettings where
    l <> r = MarginSettings
        { msTop   = on mplus msTop   l r
        , msLeft  = on mplus msLeft  l r
        , msRight = on mplus msRight l r
        }


--------------------------------------------------------------------------------
instance Monoid MarginSettings where
    mappend = (<>)
    mempty  = MarginSettings Nothing Nothing Nothing


--------------------------------------------------------------------------------
defaultMargins :: MarginSettings
defaultMargins = MarginSettings
    { msTop   = Just (A.FlexibleNum 1)
    , msLeft  = Nothing
    , msRight = Nothing
    }



--------------------------------------------------------------------------------
data Margins = Margins
    { mTop   :: Int
    , mLeft  :: Int
    , mRight :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
margins :: PresentationSettings -> Margins
margins ps = Margins
    { mLeft  = get msLeft
    , mRight = get msRight
    , mTop   = get msTop
    }
  where
    get f = fromMaybe 0 . fmap A.unFlexibleNum $ psMargins ps >>= f


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
                    intercalate ", " (map (drop 4 . show) allExts)
          where
            -- This is an approximation since we can't enumerate extensions
            -- anymore in the latest pandoc...
            allExts = Pandoc.extensionsToList $
                Pandoc.getAllExtensions "markdown"


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
type EvalSettingsMap = HMS.HashMap T.Text EvalSettings


--------------------------------------------------------------------------------
data EvalSettings = EvalSettings
    { evalCommand  :: !T.Text
    , evalReplace  :: !Bool
    , evalFragment :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
instance A.FromJSON EvalSettings where
    parseJSON = A.withObject "FromJSON EvalSettings" $ \o -> EvalSettings
        <$> o A..: "command"
        <*> o A..:? "replace" A..!= False
        <*> o A..:? "fragment" A..!= True


--------------------------------------------------------------------------------
data Slide = Slide
    { slideSpeakerNotes :: !SpeakerNotes.SpeakerNotes
    , slideContent      :: !SlideContent
    } deriving (Show)


--------------------------------------------------------------------------------
data SlideContent
    = ContentSlide (Instruction.Instructions Pandoc.Block)
    | TitleSlide   Int [Pandoc.Inline]
    deriving (Show)


--------------------------------------------------------------------------------
-- | Active slide, active fragment.
type Index = (Int, Int)


--------------------------------------------------------------------------------
getSlide :: Int -> Presentation -> Maybe Slide
getSlide sidx = (`Seq.safeIndex` sidx) . pSlides


--------------------------------------------------------------------------------
numFragments :: Slide -> Int
numFragments slide = case slideContent slide of
    ContentSlide instrs -> Instruction.numFragments instrs
    TitleSlide _ _      -> 1


--------------------------------------------------------------------------------
data ActiveFragment
    = ActiveContent Instruction.Fragment
    | ActiveTitle Pandoc.Block
    deriving (Show)


--------------------------------------------------------------------------------
activeFragment :: Presentation -> Maybe ActiveFragment
activeFragment presentation = do
    let (sidx, fidx) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    pure $ case slideContent slide of
        TitleSlide lvl is -> ActiveTitle $
            Pandoc.Header lvl Pandoc.nullAttr is
        ContentSlide instrs -> ActiveContent $
            Instruction.renderFragment fidx instrs


--------------------------------------------------------------------------------
activeSpeakerNotes :: Presentation -> SpeakerNotes.SpeakerNotes
activeSpeakerNotes presentation = fromMaybe mempty $ do
    let (sidx, _) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    pure $ slideSpeakerNotes slide


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''MarginSettings)
$(A.deriveFromJSON A.dropPrefixOptions ''PresentationSettings)
