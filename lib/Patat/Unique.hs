{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Patat.Unique
    ( Unique
    , UniqueGen
    , zeroUniqueGen
    , freshUnique
    ) where

import           Data.Hashable (Hashable)

-- | Can be used as a unique identifier.
newtype Unique = Unique Int deriving (Hashable, Eq, Ord, Show)

-- | Used to generate fresh variables.
newtype UniqueGen = UniqueGen Int deriving (Show)

zeroUniqueGen :: UniqueGen
zeroUniqueGen = UniqueGen 0

freshUnique :: UniqueGen -> (Unique, UniqueGen)
freshUnique (UniqueGen x) = (Unique x, UniqueGen (x + 1))
