-- | For background info on the spec, see the "Incremental lists" section of the
-- the pandoc manual.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Patat.Presentation.Fragment
    ( FragmentSettings (..)

    , fragmentPresentation
    , fragmentBlocks
    , fragmentBlock
    ) where

import           Control.Monad.State         (State, runState, state)
import           Data.List                   (intersperse)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as S
import           Patat.Presentation.Internal
import           Patat.Presentation.Syntax
import           Patat.Unique
import           Prelude

fragmentPresentation :: Presentation -> Presentation
fragmentPresentation presentation =
    let (pres, uniqueGen) = runState work (pUniqueGen presentation) in
    pres {pUniqueGen = uniqueGen}
  where
    work = do
        slides <- traverse fragmentSlide (pSlides presentation)
        pure presentation {pSlides = slides}

    fragmentSlide slide = case slideContent slide of
        TitleSlide   _ _     -> pure slide
        ContentSlide blocks0 -> do
            blocks1 <- fragmentBlocks fragmentSettings blocks0
            pure slide {slideContent = ContentSlide blocks1}

    settings = pSettings presentation
    fragmentSettings = FragmentSettings
        { fsIncrementalLists = fromMaybe False (psIncrementalLists settings)
        }

data FragmentSettings = FragmentSettings
    { fsIncrementalLists :: !Bool
    } deriving (Show)

type FragmentM = State UniqueGen

splitOnThreeDots :: [Block] -> [[Block]]
splitOnThreeDots blocks = case break (== threeDots) blocks of
    (pre, _ : post) -> [pre] ++ splitOnThreeDots post
    (pre, [])       -> [pre]
  where
    threeDots = Para $ intersperse Space $ replicate 3 (Str ".")

fragmentBlocks
    :: FragmentSettings -> [Block] -> FragmentM [Block]
fragmentBlocks fs blocks = case splitOnThreeDots blocks of
    [] -> pure []
    [_] -> concat <$> traverse (fragmentBlock fs) blocks
    sections0@(_ : _) -> do
        counterID <- CounterID <$> state freshUnique
        sections1 <- traverse (fragmentBlocks fs) sections0
        let pauses = length sections1 - 1
            triggers = case sections1 of
                [] -> replicate pauses counterID
                (sh : st) -> blocksTriggers sh ++
                    [c | s <- st, c <- counterID : blocksTriggers s]
        pure $ pure $ Fragmented ConcatWrapper $ Fragment
            counterID
            triggers
            [(S.fromList [i .. pauses], s) | (i, s) <- zip [0 ..] sections1]

fragmentBlock :: FragmentSettings -> Block -> FragmentM [Block]
fragmentBlock _fs (Para inlines) = pure [Para inlines]

fragmentBlock fs (BulletList bs0) =
    fragmentList fs (fsIncrementalLists fs) BulletListWrapper bs0

fragmentBlock fs (OrderedList attr bs0) =
    fragmentList fs (fsIncrementalLists fs) (OrderedListWrapper attr) bs0

fragmentBlock fs (BlockQuote [BulletList bs0]) =
    fragmentList fs (not $ fsIncrementalLists fs) BulletListWrapper bs0

fragmentBlock fs (BlockQuote [OrderedList attr bs0]) =
    fragmentList fs (not $ fsIncrementalLists fs) (OrderedListWrapper attr) bs0

fragmentBlock _ block@(BlockQuote {})     = pure [block]

fragmentBlock _ block@(Header {})         = pure [block]
fragmentBlock _ block@(Plain {})          = pure [block]
fragmentBlock _ block@(CodeBlock {})      = pure [block]
fragmentBlock _ block@(RawBlock {})       = pure [block]
fragmentBlock _ block@(DefinitionList {}) = pure [block]
fragmentBlock _ block@(Table {})          = pure [block]
fragmentBlock _ block@(Div {})            = pure [block]
fragmentBlock _ block@HorizontalRule      = pure [block]
fragmentBlock _ block@(LineBlock {})      = pure [block]
fragmentBlock _ block@(Figure {})         = pure [block]
fragmentBlock _ block@(VarBlock {})       = pure [block]
fragmentBlock _ block@(SpeakerNote {})    = pure [block]
fragmentBlock _ block@(Config {})         = pure [block]
fragmentBlock _ block@(Fragmented {})     = pure [block]  -- Should not happen

fragmentList
    :: FragmentSettings   -- ^ Global settings
    -> Bool               -- ^ Fragment THIS list?
    -> FragmentWrapper    -- ^ List constructor
    -> [[Block]]          -- ^ List items
    -> FragmentM [Block]  -- ^ Resulting list
fragmentList fs fragmentThisList fw items0 = do
    items1 <- traverse (fragmentBlocks fs) items0
    case fragmentThisList of
        False -> pure $ fragmentWrapper fw items1
        True -> do
            counterID <- CounterID <$> state freshUnique
            let triggers = [c | s <- items1, c <- counterID : blocksTriggers s]
                pauses   = length items1
            pure $ pure $ Fragmented fw $ Fragment
                counterID
                triggers
                [ (S.fromList [i .. pauses], s)
                | (i, s) <- zip [1 ..] items1
                ]
