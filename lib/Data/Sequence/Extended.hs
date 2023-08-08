module Data.Sequence.Extended
    ( module Data.Sequence
    , cons
    , safeIndex
    ) where

import Data.Sequence

cons :: a -> Seq a -> Seq a
cons x xs = singleton x <> xs

safeIndex :: Seq a -> Int -> Maybe a
safeIndex s n
    | n < 0                       = Nothing
    | n >= Data.Sequence.length s = Nothing
    | otherwise                   = Just $ index s n
