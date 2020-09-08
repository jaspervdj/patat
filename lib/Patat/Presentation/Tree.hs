--------------------------------------------------------------------------------
-- | In this module, we model the contents of a slide as a rose tree.  This
-- allows us to do fragments that appear bit by bit, as well as manipulating
-- the structure of these fragments later.
module Patat.Presentation.Tree
    ( Instruction (..)
    , numPauses
    , renderAt
    , apply
    ) where

import Debug.Trace
import qualified Text.Pandoc as Pandoc

data Instruction a
    -- Pause.
    = Pause
    -- Append items.
    | Append [a]
    -- Modify the last block with further instructions.
    | ModifyLast (Instruction a)
    deriving (Show)

numPauses :: [Instruction a] -> Int
numPauses = length . filter isPause

isPause :: Instruction a -> Bool
isPause Pause = True
isPause (Append _) = False
isPause (ModifyLast i) = isPause i

renderAt :: Int -> [Instruction Pandoc.Block] -> [Pandoc.Block]
renderAt = go []
  where
    go acc _ []               = acc
    go acc n (instr : instrs)
        | isPause instr = if n <= 0 then acc else go acc (n - 1) instrs
        | otherwise     =
            let acc' = goBlocks instr acc in
            trace ("INSTR " ++ show instr) $
            trace ("ACC " ++ show acc') $
            go (goBlocks instr acc) n instrs

apply :: Instruction Pandoc.Block -> [Pandoc.Block] -> [Pandoc.Block]
apply = goBlocks

goBlocks :: Instruction Pandoc.Block -> [Pandoc.Block] -> [Pandoc.Block]
goBlocks Pause xs = xs
goBlocks (Append ys) xs = xs ++ ys
goBlocks (ModifyLast f) xs
    | null xs   = xs  -- Shouldn't happen
    | otherwise = modifyLast (goBlock f) xs

goBlock :: Instruction Pandoc.Block -> Pandoc.Block -> Pandoc.Block
goBlock Pause x = x
goBlock (Append ys) block = case block of
    Pandoc.BulletList xs -> Pandoc.BulletList $ xs ++ [ys]
    Pandoc.OrderedList attr xs -> Pandoc.OrderedList attr $ xs ++ [ys]
    _ -> block
goBlock (ModifyLast f) block = case block of
    Pandoc.BulletList xs -> Pandoc.BulletList $ modifyLast (goBlocks f) xs
    Pandoc.OrderedList attr xs ->
        Pandoc.OrderedList attr $ modifyLast (goBlocks f) xs
    _ -> block

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast f (x : y : zs) = x : modifyLast f (y : zs)
modifyLast f (x : [])     = [f x]
modifyLast _ []           = []
