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

    , getSettings
    , activeSettings

    , Size
    , getPresentationSize
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended            as A
import           Data.Maybe                     (fromMaybe)
import           Data.Sequence.Extended         (Seq)
import qualified Data.Sequence.Extended         as Seq
import           Patat.EncodingFallback         (EncodingFallback)
import qualified Patat.Presentation.Comments    as Comments
import qualified Patat.Presentation.Instruction as Instruction
import           Patat.Presentation.Settings
import           Patat.Size
import           Prelude
import qualified Skylighting                    as Skylighting
import qualified Text.Pandoc                    as Pandoc


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
    , pBreadcrumbs      :: !(Seq Breadcrumbs)           -- One for each slide.
    , pSlideSettings    :: !(Seq PresentationSettings)  -- One for each slide.
    , pActiveFragment   :: !Index
    , pSyntaxMap        :: !Skylighting.SyntaxMap
    } deriving (Show)


--------------------------------------------------------------------------------
data Margins = Margins
    { mTop   :: Int
    , mLeft  :: Int
    , mRight :: Int
    } deriving (Show)


--------------------------------------------------------------------------------
margins :: PresentationSettings -> Margins
margins ps = Margins
    { mLeft  = get 0 msLeft
    , mRight = get 0 msRight
    , mTop   = get 1 msTop
    }
  where
    get def f = fromMaybe def . fmap A.unFlexibleNum $ psMargins ps >>= f


--------------------------------------------------------------------------------
data Slide = Slide
    { slideComment :: !Comments.Comment
    , slideContent :: !SlideContent
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
activeSpeakerNotes :: Presentation -> Comments.SpeakerNotes
activeSpeakerNotes presentation = fromMaybe mempty $ do
    let (sidx, _) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    pure . Comments.cSpeakerNotes $ slideComment slide


--------------------------------------------------------------------------------
getSettings :: Int -> Presentation -> PresentationSettings
getSettings sidx pres =
    fromMaybe mempty (Seq.safeIndex (pSlideSettings pres) sidx) <>
    pSettings pres


--------------------------------------------------------------------------------
activeSettings :: Presentation -> PresentationSettings
activeSettings pres =
    let (sidx, _) = pActiveFragment pres in getSettings sidx pres


--------------------------------------------------------------------------------
getPresentationSize :: Presentation -> IO Size
getPresentationSize pres = do
    term <- getTerminalSize
    let rows = fromMaybe (sRows term) $ A.unFlexibleNum <$> psRows settings
        cols = fromMaybe (sCols term) $ A.unFlexibleNum <$> psColumns settings
    pure $ Size {sRows = rows, sCols = cols}
  where
    settings = activeSettings pres
