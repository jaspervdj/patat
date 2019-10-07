--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Text.Pandoc.Extended
    ( module Text.Pandoc

    , plainToPara
    , newlineToSpace
    ) where


--------------------------------------------------------------------------------
import           Data.Data.Extended (grecT)
import           Text.Pandoc
import           Prelude


--------------------------------------------------------------------------------
plainToPara :: [Block] -> [Block]
plainToPara = map $ \case
    Plain inlines -> Para inlines
    block         -> block


--------------------------------------------------------------------------------
newlineToSpace :: [Inline] -> [Inline]
newlineToSpace = grecT $ \case
    SoftBreak -> Space
    LineBreak -> Space
    inline    -> inline
