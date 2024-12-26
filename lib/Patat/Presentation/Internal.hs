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
    , activeVars

    , getSettings
    , activeSettings

    , Size
    , getPresentationSize

    , updateVar
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended            as A
import qualified Data.HashMap.Strict            as HMS
import qualified Data.HashSet                   as HS
import           Data.Maybe                     (fromMaybe)
import           Data.Sequence.Extended         (Seq)
import qualified Data.Sequence.Extended         as Seq
import           Patat.EncodingFallback         (EncodingFallback)
import qualified Patat.Eval.Internal            as Eval
import qualified Patat.Presentation.Comments    as Comments
import qualified Patat.Presentation.Instruction as Instruction
import           Patat.Presentation.Settings
import           Patat.Presentation.Syntax
import           Patat.Size
import           Patat.Transition               (TransitionGen)
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
    , pBreadcrumbs      :: !(Seq Breadcrumbs)            -- One for each slide.
    , pSlideSettings    :: !(Seq PresentationSettings)   -- One for each slide.
    , pTransitionGens   :: !(Seq (Maybe TransitionGen))  -- One for each slide.
    , pActiveFragment   :: !Index
    , pSyntaxMap        :: !Skylighting.SyntaxMap
    , pEvalBlocks       :: !Eval.EvalBlocks
    , pVarGen           :: !Instruction.VarGen
    , pVars             :: !(HMS.HashMap Instruction.Var [Block])
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
    { slideComment :: !Comments.Comment
    , slideContent :: !SlideContent
    } deriving (Show)


--------------------------------------------------------------------------------
data SlideContent
    = ContentSlide (Instruction.Instructions Block)
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
        ContentSlide instrs -> ActiveContent $
            Instruction.renderFragment resolve $
            Instruction.beforePause fidx instrs
  where
    resolve var = fromMaybe [] $ HMS.lookup var (pVars presentation)


--------------------------------------------------------------------------------
activeSpeakerNotes :: Presentation -> Comments.SpeakerNotes
activeSpeakerNotes presentation = fromMaybe mempty $ do
    let (sidx, _) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    pure . Comments.cSpeakerNotes $ slideComment slide


--------------------------------------------------------------------------------
activeVars :: Presentation -> HS.HashSet Instruction.Var
activeVars presentation = fromMaybe HS.empty $ do
    let (sidx, fidx) = pActiveFragment presentation
    slide <- getSlide sidx presentation
    case slideContent slide of
        TitleSlide _ _ -> Nothing
        ContentSlide instrs -> pure $ Instruction.variables $
            Instruction.beforePause fidx instrs


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
updateVar :: Instruction.Var -> [Block] -> Presentation -> Presentation
updateVar var blocks pres = pres {pVars = HMS.insert var blocks $ pVars pres}
