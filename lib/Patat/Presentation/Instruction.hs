--------------------------------------------------------------------------------
-- | The Pandoc AST is not extensible, so we need to use another way to model
-- different parts of slides that we want to appear bit by bit.
--
-- We do this by modelling a slide as a list of instructions, that manipulate
-- the contents on a slide in a (for now) very basic way.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Patat.Presentation.Instruction
    ( Instructions
    , fromList
    , toList

    , Var
    , VarGen
    , zeroVarGen
    , freshVar

    , Instruction (..)
    , beforePause
    , numFragments

    , Fragment (..)
    , renderFragment
    ) where

import           Data.List                 (foldl')
import           Patat.Presentation.Syntax

newtype Instructions a = Instructions {unInstructions :: [Instruction a]}
    deriving (Show)

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
isPause Pause          = True
isPause (Append _)     = False
isPause Delete         = False
isPause (ModifyLast i) = isPause i

numPauses :: Instructions a -> Int
numPauses (Instructions xs) = length $ filter isPause xs

beforePause :: Int -> Instructions a -> Instructions a
beforePause n = Instructions . go 0 . unInstructions
  where
    go _ []          = []
    go i (Pause : t) = if i >= n then [] else go (i + 1) t
    go i (h     : t) = h : go i t

numFragments :: Instructions a -> Int
numFragments = succ . numPauses

newtype Fragment = Fragment {unFragment :: [Block]} deriving (Show)

renderFragment
    :: (Var -> [Block]) -> Instructions Block -> Fragment
renderFragment resolve = \instrs -> Fragment $ foldl'
    (\acc instr -> goBlocks resolve instr acc) [] (unInstructions instrs)

goBlocks
    :: (Var -> [Block]) -> Instruction Block -> [Block]
    -> [Block]
goBlocks _ Pause xs = xs
goBlocks _ (Append ys) xs = xs ++ ys
goBlocks _ Delete xs = sinit xs
goBlocks resolve (ModifyLast f) xs
    | null xs   = xs  -- Shouldn't happen unless instructions are malformed.
    | otherwise = modifyLast (goBlock resolve f) xs

goBlock :: (Var -> [Block]) -> Instruction Block -> Block -> Block
goBlock _ Pause x = x
goBlock _ (Append ys) block = case block of
    -- We can only append to a few specific block types for now.
    BulletList xs       -> BulletList $ xs ++ [ys]
    OrderedList attr xs -> OrderedList attr $ xs ++ [ys]
    _                   -> block
goBlock _ Delete block = case block of
    -- We can only delete from a few specific block types for now.
    BulletList xs       -> BulletList $ sinit xs
    OrderedList attr xs -> OrderedList attr $ sinit xs
    _                   -> block
goBlock resolve (ModifyLast f) block = case block of
    -- We can only modify the last content of a few specific block types for
    -- now.
    BulletList xs       -> BulletList $ modifyLast (goBlocks resolve f) xs
    OrderedList attr xs -> OrderedList attr $ modifyLast (goBlocks resolve f) xs
    _                   -> block

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast f (x : y : zs) = x : modifyLast f (y : zs)
modifyLast f (x : [])     = [f x]
modifyLast _ []           = []

sinit :: [a] -> [a]
sinit xs = if null xs then [] else init xs
