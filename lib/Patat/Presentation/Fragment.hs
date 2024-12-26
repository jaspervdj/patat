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
import           Patat.Presentation.Syntax
import           Prelude
import qualified Text.Pandoc                    as Pandoc

data FragmentSettings = FragmentSettings
    { fsIncrementalLists :: !Bool
    } deriving (Show)

fragmentInstructions
    :: FragmentSettings
    -> Instructions Block -> Instructions Block
fragmentInstructions fs = fromList . concatMap fragmentInstruction . toList
  where
    fragmentInstruction Pause = [Pause]
    fragmentInstruction (Append []) = [Append []]
    fragmentInstruction (Append xs) = fragmentBlocks fs xs
    fragmentInstruction (AppendVar v) = [AppendVar v]
    fragmentInstruction Delete = [Delete]
    fragmentInstruction (ModifyLast f) = map ModifyLast $ fragmentInstruction f

fragmentBlocks
    :: FragmentSettings -> [Block] -> [Instruction Block]
fragmentBlocks = concatMap . fragmentBlock

fragmentBlock :: FragmentSettings -> Block -> [Instruction Block]
fragmentBlock _fs block@(Para inlines)
    | inlines == threeDots = [Pause]
    | otherwise            = [Append [block]]
  where
    threeDots = intersperse Pandoc.Space $ replicate 3 (Pandoc.Str ".")

fragmentBlock fs (BulletList bs0) =
    fragmentList fs (fsIncrementalLists fs) BulletList bs0

fragmentBlock fs (OrderedList attr bs0) =
    fragmentList fs (fsIncrementalLists fs) (OrderedList attr) bs0

fragmentBlock fs (BlockQuote [BulletList bs0]) =
    fragmentList fs (not $ fsIncrementalLists fs) BulletList bs0

fragmentBlock fs (BlockQuote [OrderedList attr bs0]) =
    fragmentList fs (not $ fsIncrementalLists fs) (OrderedList attr) bs0

fragmentBlock _ block@(BlockQuote {})     = [Append [block]]

fragmentBlock _ block@(Header {})         = [Append [block]]
fragmentBlock _ block@(Plain {})          = [Append [block]]
fragmentBlock _ block@(CodeBlock {})      = [Append [block]]
fragmentBlock _ block@(RawBlock {})       = [Append [block]]
fragmentBlock _ block@(DefinitionList {}) = [Append [block]]
fragmentBlock _ block@(Table {})          = [Append [block]]
fragmentBlock _ block@(Div {})            = [Append [block]]
fragmentBlock _ block@HorizontalRule      = [Append [block]]
fragmentBlock _ block@(LineBlock {})      = [Append [block]]
fragmentBlock _ block@(Figure {})         = [Append [block]]

fragmentList
    :: FragmentSettings      -- ^ Global settings
    -> Bool                  -- ^ Fragment THIS list?
    -> ([[Block]] -> Block)  -- ^ List constructor
    -> [[Block]]             -- ^ List items
    -> [Instruction Block]   -- ^ Resulting list
fragmentList fs fragmentThisList constructor items =
    -- Insert the new list, initially empty.
    (if fragmentThisList then [Pause] else []) ++
    [Append [constructor []]] ++
    (map ModifyLast $
        (if fragmentThisList then intercalate [Pause] else concat) $
        map fragmentItem items)
  where
    -- The fragmented list per list item.
    fragmentItem :: [Block] -> [Instruction Block]
    fragmentItem item =
        -- Append a new item to the list so we can start adding
        -- content there.
        Append [] :
        -- Modify this new item to add the content.
        map ModifyLast (fragmentBlocks fs item)
