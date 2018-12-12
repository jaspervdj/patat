-- | For background info on the spec, see the "Incremental lists" section of the
-- the pandoc manual.
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Patat.Presentation.Fragment
    ( FragmentSettings (..)
    , fragmentBlocks
    , fragmentBlock
    ) where

import           Data.Foldable    (Foldable)
import           Data.List        (foldl', intersperse)
import           Data.Maybe       (fromMaybe)
import           Data.Traversable (Traversable)
import           Prelude
import qualified Text.Pandoc      as Pandoc

data FragmentSettings = FragmentSettings
    { fsIncrementalLists :: !Bool
    } deriving (Show)

-- fragmentBlocks :: [Pandoc.Block] -> [[Pandoc.Block]]
-- fragmentBlocks = NonEmpty.toList . joinFragmentedBlocks . map fragmentBlock
fragmentBlocks :: FragmentSettings -> [Pandoc.Block] -> [[Pandoc.Block]]
fragmentBlocks fs blocks0 =
    case joinFragmentedBlocks (map (fragmentBlock fs) blocks0) of
        Unfragmented  bs -> [bs]
        Fragmented xs bs -> map (fromMaybe []) xs ++ [fromMaybe [] bs]

-- | This is all the ways we can "present" a block, after splitting in
-- fragments.
--
-- In the simplest (and most common case) a block can only be presented in a
-- single way ('Unfragmented').
--
-- Alternatively, we might want to show different (partial) versions of the
-- block first before showing the final complete one.  These partial or complete
-- versions can be empty, hence the 'Maybe'.
--
-- For example, imagine that we display the following bullet list incrementally:
--
-- > [1, 2, 3]
--
-- Then we would get something like:
--
-- > Fragmented [Nothing, Just [1], Just [1, 2]] (Just [1, 2, 3])
data Fragmented a
    = Unfragmented a
    | Fragmented [Maybe a] (Maybe a)
    deriving (Functor, Foldable, Show, Traversable)

fragmentBlock :: FragmentSettings -> Pandoc.Block -> Fragmented Pandoc.Block
fragmentBlock _fs block@(Pandoc.Para inlines)
    | inlines == threeDots = Fragmented [Nothing] Nothing
    | otherwise            = Unfragmented block
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

fragmentBlock _ block@(Pandoc.BlockQuote _)     = Unfragmented block

fragmentBlock _ block@(Pandoc.Header _ _ _)     = Unfragmented block
fragmentBlock _ block@(Pandoc.Plain _)          = Unfragmented block
fragmentBlock _ block@(Pandoc.CodeBlock _ _)    = Unfragmented block
fragmentBlock _ block@(Pandoc.RawBlock _ _)     = Unfragmented block
fragmentBlock _ block@(Pandoc.DefinitionList _) = Unfragmented block
fragmentBlock _ block@(Pandoc.Table _ _ _ _ _)  = Unfragmented block
fragmentBlock _ block@(Pandoc.Div _ _)          = Unfragmented block
fragmentBlock _ block@Pandoc.HorizontalRule     = Unfragmented block
fragmentBlock _ block@Pandoc.Null               = Unfragmented block

#if MIN_VERSION_pandoc(1,18,0)
fragmentBlock _ block@(Pandoc.LineBlock _)      = Unfragmented block
#endif

joinFragmentedBlocks :: [Fragmented block] -> Fragmented [block]
joinFragmentedBlocks =
    foldl' append (Unfragmented [])
  where
    append (Unfragmented xs) (Unfragmented y) =
        Unfragmented (xs ++ [y])

    append (Fragmented xs x) (Unfragmented y) =
        Fragmented xs (appendMaybe x (Just y))

    append (Unfragmented x) (Fragmented ys y) =
        Fragmented
            [appendMaybe (Just x) y' | y' <- ys]
            (appendMaybe (Just x) y)

    append (Fragmented xs x) (Fragmented ys y) =
        Fragmented
            (xs ++ [appendMaybe x y' | y' <- ys])
            (appendMaybe x y)

    appendMaybe :: Maybe [a] -> Maybe a -> Maybe [a]
    appendMaybe Nothing   Nothing  = Nothing
    appendMaybe Nothing   (Just x) = Just [x]
    appendMaybe (Just xs) Nothing  = Just xs
    appendMaybe (Just xs) (Just x) = Just (xs ++ [x])

fragmentList
    :: FragmentSettings                    -- ^ Global settings
    -> Bool                                -- ^ Fragment THIS list?
    -> ([[Pandoc.Block]] -> Pandoc.Block)  -- ^ List constructor
    -> [[Pandoc.Block]]                    -- ^ List items
    -> Fragmented Pandoc.Block             -- ^ Resulting list
fragmentList fs fragmentThisList constructor blocks0 =
    fmap constructor fragmented
  where
    -- The fragmented list per list item.
    items :: [Fragmented [Pandoc.Block]]
    items = map (joinFragmentedBlocks . map (fragmentBlock fs)) blocks0

    fragmented :: Fragmented [[Pandoc.Block]]
    fragmented = joinFragmentedBlocks $
        map (if fragmentThisList then insertPause else id) items

    insertPause :: Fragmented a -> Fragmented a
    insertPause (Unfragmented x)  = Fragmented [Nothing] (Just x)
    insertPause (Fragmented xs x) = Fragmented (Nothing : xs) x
