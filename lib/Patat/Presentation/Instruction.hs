--------------------------------------------------------------------------------
-- | The Pandoc AST is not extensible, so we need to use another way to model
-- different parts of slides that we want to appear bit by bit.
--
-- We do this by modelling a slide as a list of instructions, that manipulate
-- the contents on a slide in a (for now) very basic way.
module Patat.Presentation.Instruction
    ( Instructions
    , fromList
    , toList

    , Instruction (..)
    , numFragments

    , Fragment (..)
    , renderFragment
    ) where

import qualified Text.Pandoc as Pandoc

newtype Instructions a = Instructions [Instruction a] deriving (Show)

-- A smart constructor that guarantees some invariants:
--
--  *  No consecutive pauses.
--  *  All pauses moved to the top level.
--  *  No pauses at the end.
fromList :: [Instruction a] -> Instructions a
fromList = Instructions . go
  where
    go instrs = case break (not . isPause) instrs of
        (_, [])             -> []
        (_ : _, remainder)  -> Pause : go remainder
        ([], x : remainder) -> x : go remainder

toList :: Instructions a -> [Instruction a]
toList (Instructions xs) = xs

data Instruction a
    -- Pause.
    = Pause
    -- Append items.
    | Append [a]
    -- Remove the last item.
    | Delete
    -- Modify the last block with the provided instruction.
    | ModifyLast (Instruction a)
    deriving (Show)

isPause :: Instruction a -> Bool
isPause Pause = True
isPause (Append _) = False
isPause Delete = False
isPause (ModifyLast i) = isPause i

numPauses :: Instructions a -> Int
numPauses (Instructions xs) = length $ filter isPause xs

numFragments :: Instructions a -> Int
numFragments = succ . numPauses

newtype Fragment = Fragment [Pandoc.Block] deriving (Show)

renderFragment :: Int -> Instructions Pandoc.Block -> Fragment
renderFragment = \n (Instructions instrs) -> Fragment $ go [] n instrs
  where
    go acc _ []         = acc
    go acc n (Pause : instrs) = if n <= 0 then acc else go acc (n - 1) instrs
    go acc n (instr : instrs) = go (goBlocks instr acc) n instrs

goBlocks :: Instruction Pandoc.Block -> [Pandoc.Block] -> [Pandoc.Block]
goBlocks Pause xs = xs
goBlocks (Append ys) xs = xs ++ ys
goBlocks Delete xs = sinit xs
goBlocks (ModifyLast f) xs
    | null xs   = xs  -- Shouldn't happen unless instructions are malformed.
    | otherwise = modifyLast (goBlock f) xs

goBlock :: Instruction Pandoc.Block -> Pandoc.Block -> Pandoc.Block
goBlock Pause x = x
goBlock (Append ys) block = case block of
    -- We can only append to a few specific block types for now.
    Pandoc.BulletList xs -> Pandoc.BulletList $ xs ++ [ys]
    Pandoc.OrderedList attr xs -> Pandoc.OrderedList attr $ xs ++ [ys]
    _ -> block
goBlock Delete block = case block of
    -- We can only append to a few specific block types for now.
    Pandoc.BulletList xs -> Pandoc.BulletList $ sinit xs
    Pandoc.OrderedList attr xs -> Pandoc.OrderedList attr $ sinit xs
    _ -> block
goBlock (ModifyLast f) block = case block of
    -- We can only modify the last content of a few specific block types for
    -- now.
    Pandoc.BulletList xs -> Pandoc.BulletList $ modifyLast (goBlocks f) xs
    Pandoc.OrderedList attr xs ->
        Pandoc.OrderedList attr $ modifyLast (goBlocks f) xs
    _ -> block

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast f (x : y : zs) = x : modifyLast f (y : zs)
modifyLast f (x : [])     = [f x]
modifyLast _ []           = []

sinit :: [a] -> [a]
sinit xs = if null xs then [] else init xs
