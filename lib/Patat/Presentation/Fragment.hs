-- | For background info on the spec, see the "Incremental lists" section of the
-- the pandoc manual.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Patat.Presentation.Fragment
    ( FragmentSettings (..)

    , fragmentInstructions
    , fragmentBlocks
    , fragmentBlock
    ) where

import           Data.List                      (intersperse, intercalate)
import           Patat.Presentation.Instruction
import           Prelude
import qualified Text.Pandoc                    as Pandoc

data FragmentSettings = FragmentSettings
    { fsIncrementalLists :: !Bool
    } deriving (Show)

fragmentInstructions
    :: FragmentSettings
    -> Instructions Pandoc.Block -> Instructions Pandoc.Block
fragmentInstructions fs = fromList . concatMap fragmentInstruction . toList
  where
    fragmentInstruction Pause = [Pause]
    fragmentInstruction (Append []) = [Append []]
    fragmentInstruction (Append xs) = fragmentBlocks fs xs
    fragmentInstruction Delete = [Delete]
    fragmentInstruction (ModifyLast f) = map ModifyLast $ fragmentInstruction f

fragmentBlocks
    :: FragmentSettings -> [Pandoc.Block] -> [Instruction Pandoc.Block]
fragmentBlocks = concatMap . fragmentBlock

fragmentBlock :: FragmentSettings -> Pandoc.Block -> [Instruction Pandoc.Block]
fragmentBlock _fs block@(Pandoc.Para inlines)
    | inlines == threeDots = [Pause]
    | otherwise            = [Append [block]]
  where
    threeDots = intersperse Pandoc.Space $ replicate 3 (Pandoc.Str ".")

fragmentBlock fs (Pandoc.BulletList bs0) =
    fragmentList fs (fsIncrementalLists fs) Pandoc.BulletList bs0

fragmentBlock fs (Pandoc.OrderedList attr bs0) =
    fragmentList fs (fsIncrementalLists fs) (Pandoc.OrderedList attr) bs0

fragmentBlock fs (Pandoc.BlockQuote [Pandoc.BulletList bs0]) =
    fragmentList fs (not $ fsIncrementalLists fs) Pandoc.BulletList bs0

fragmentBlock fs (Pandoc.BlockQuote [Pandoc.OrderedList attr bs0]) =
    fragmentList fs (not $ fsIncrementalLists fs) (Pandoc.OrderedList attr) bs0

fragmentBlock _ block@(Pandoc.BlockQuote {})     = [Append [block]]

fragmentBlock _ block@(Pandoc.Header {})         = [Append [block]]
fragmentBlock _ block@(Pandoc.Plain {})          = [Append [block]]
fragmentBlock _ block@(Pandoc.CodeBlock {})      = [Append [block]]
fragmentBlock _ block@(Pandoc.RawBlock {})       = [Append [block]]
fragmentBlock _ block@(Pandoc.DefinitionList {}) = [Append [block]]
fragmentBlock _ block@(Pandoc.Table {})          = [Append [block]]
fragmentBlock _ block@(Pandoc.Div {})            = [Append [block]]
fragmentBlock _ block@Pandoc.HorizontalRule      = [Append [block]]
fragmentBlock _ block@(Pandoc.LineBlock {})      = [Append [block]]
fragmentBlock _ block@(Pandoc.Figure {})         = [Append [block]]

fragmentList
    :: FragmentSettings                    -- ^ Global settings
    -> Bool                                -- ^ Fragment THIS list?
    -> ([[Pandoc.Block]] -> Pandoc.Block)  -- ^ List constructor
    -> [[Pandoc.Block]]                    -- ^ List items
    -> [Instruction Pandoc.Block]          -- ^ Resulting list
fragmentList fs fragmentThisList constructor items =
    -- Insert the new list, initially empty.
    (if fragmentThisList then [Pause] else []) ++
    [Append [constructor []]] ++
    (map ModifyLast $
        (if fragmentThisList then intercalate [Pause] else concat) $
        map fragmentItem items)
  where
    -- The fragmented list per list item.
    fragmentItem :: [Pandoc.Block] -> [Instruction Pandoc.Block]
    fragmentItem item =
        -- Append a new item to the list so we can start adding
        -- content there.
        Append [] :
        -- Modify this new item to add the content.
        map ModifyLast (fragmentBlocks fs item)
