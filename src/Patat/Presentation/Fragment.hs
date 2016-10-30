module Patat.Presentation.Fragment
    ( fragmentBlocks
    ) where

import qualified Text.Pandoc as Pandoc

fragmentBlock :: Pandoc.Block -> [Pandoc.Block]
fragmentBlock (Pandoc.Plain inlines) =
    map Pandoc.Plain (fragmentInlines inlines)

fragmentBlock (Pandoc.Para inlines) =
    map Pandoc.Para (fragmentInlines inlines)

fragmentBlock b = error $ "fragmentBlock " ++ show b

fragmentBlocks :: [Pandoc.Block] -> [[Pandoc.Block]]
fragmentBlocks []       = [[]]
fragmentBlocks (b : bs) = case viewLast (fragmentBlock b) of
    Nothing       -> fragmentBlocks bs
    Just (fgs, l) -> map return fgs ++ map (l :) (fragmentBlocks bs)

fragmentInline :: Pandoc.Inline -> [Pandoc.Inline]
fragmentInline x = [x]

fragmentInlines :: [Pandoc.Inline] -> [[Pandoc.Inline]]
fragmentInlines [] = [[]]
fragmentInlines (x : xs)
    | isDelimiter x =
        [] : [ys | ys <- fragmentInlines xs]
    | otherwise = case viewLast (fragmentInline x) of
        Nothing       -> fragmentInlines xs
        Just (fgs, l) -> map return fgs ++ map (l :) (fragmentInlines xs)

isDelimiter :: Pandoc.Inline -> Bool
isDelimiter (Pandoc.Link _ _ (target, _)) = target == "#fragment"
isDelimiter _                             = False

viewLast :: [a] -> Maybe ([a], a)
viewLast = go []
  where
    go _   []       = Nothing
    go acc (x : []) = Just (reverse acc, x)
    go acc (x : xs) = go (x : acc) xs
