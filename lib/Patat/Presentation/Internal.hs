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

    , EvalSettingsMap (..)
    , EvalSettings (..)

    , Slide (..)
    , SlideContent (..)
    , Index

    , getSlide
    , numFragments

    , ActiveFragment (..)
    , activeFragment
    , activeSpeakerNotes
    , activeVars

    , getSettings
    , activeSettings

    , Size
    , getPresentationSize

    , updateVar
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended             as A
import qualified Data.HashMap.Strict             as HMS
import qualified Data.HashSet                    as HS
import           Data.Maybe                      (fromMaybe)
import           Data.Sequence.Extended          (Seq)
import qualified Data.Sequence.Extended          as Seq
import           Patat.EncodingFallback          (EncodingFallback)
import qualified Patat.Eval.Internal             as Eval
import           Patat.Presentation.Settings
import qualified Patat.Presentation.SpeakerNotes as SpeakerNotes
import           Patat.Presentation.Syntax
import           Patat.Size
import           Patat.Transition                (TransitionGen)
import           Patat.Unique
import           Prelude
import qualified Skylighting                     as Skylighting
import qualified Text.Pandoc                     as Pandoc


--------------------------------------------------------------------------------
type Breadcrumbs = [(Int, [Inline])]


--------------------------------------------------------------------------------
data Presentation = Presentation
    { pFilePath         :: !FilePath
    , pEncodingFallback :: !EncodingFallback
    , pTitle            :: ![Inline]
    , pAuthor           :: ![Inline]
    , pSettings         :: !PresentationSettings
    , pSlides           :: !(Seq Slide)
    , pBreadcrumbs      :: !(Seq Breadcrumbs)            -- One for each slide.
    , pSlideSettings    :: !(Seq PresentationSettings)   -- One for each slide.
    , pTransitionGens   :: !(Seq (Maybe TransitionGen))  -- One for each slide.
    , pActiveFragment   :: !Index
    , pSyntaxMap        :: !Skylighting.SyntaxMap
    , pEvalBlocks       :: !Eval.EvalBlocks
    , pUniqueGen        :: !UniqueGen
    , pVars             :: !(HMS.HashMap Var [Block])
    }


--------------------------------------------------------------------------------
data Margins = Margins
    { mTop   :: AutoOr Int
    , mLeft  :: AutoOr Int
    , mRight :: AutoOr Int
    } deriving (Show)


--------------------------------------------------------------------------------
margins :: PresentationSettings -> Margins
margins ps = Margins
    { mLeft  = get 0 msLeft
    , mRight = get 0 msRight
    , mTop   = get 1 msTop
    }
  where
    get def f = case psMargins ps >>= f of
        Just Auto         -> Auto
        Nothing           -> NotAuto def
        Just (NotAuto fn) -> NotAuto $ A.unFlexibleNum fn

--------------------------------------------------------------------------------
data Slide = Slide
    { slideSpeakerNotes :: !SpeakerNotes.SpeakerNotes
    , slideSettings     :: !(Either String PresentationSettings)
    , slideContent      :: !SlideContent
    } deriving (Show)


--------------------------------------------------------------------------------
data SlideContent
    = ContentSlide [Block]
    | TitleSlide   Int [Inline]
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
    ContentSlide blocks -> blocksRevealSteps blocks
    TitleSlide _ _      -> 1


--------------------------------------------------------------------------------
data ActiveFragment
    = ActiveContent
        [Block]
        (HS.HashSet Var)
        RevealState
    | ActiveTitle Block
    deriving (Show)


--------------------------------------------------------------------------------
activeFragment :: Presentation -> Maybe ActiveFragment
activeFragment presentation = do
    let (sidx, fidx) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    pure $ case slideContent slide of
        TitleSlide lvl is -> ActiveTitle $
            Header lvl Pandoc.nullAttr is
        ContentSlide blocks ->
            let vars = variables $ blocksReveal revealState blocks
                revealState = blocksRevealStep fidx blocks in
            ActiveContent blocks vars revealState


--------------------------------------------------------------------------------
activeSpeakerNotes :: Presentation -> SpeakerNotes.SpeakerNotes
activeSpeakerNotes presentation = fromMaybe mempty $ do
    let (sidx, _) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    pure $ slideSpeakerNotes slide


--------------------------------------------------------------------------------
activeVars :: Presentation -> HS.HashSet Var
activeVars presentation = case activeFragment presentation of
    Just (ActiveContent _ vars _) -> vars
    _                             -> mempty


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


--------------------------------------------------------------------------------
updateVar :: Var -> [Block] -> Presentation -> Presentation
updateVar var blocks pres = pres {pVars = HMS.insert var blocks $ pVars pres}
