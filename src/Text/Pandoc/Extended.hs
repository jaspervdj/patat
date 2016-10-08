--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
module Text.Pandoc.Extended
    ( module Text.Pandoc

    , plainToPara
    , newlineToSpace
    , metaToJson
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson         as A
import           Data.Data.Extended (grecT)
import qualified Data.Map           as M
import           Data.Monoid        (mempty)
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


--------------------------------------------------------------------------------
-- | Convert Pandoc's internal metadata value format to JSON.  This makes
-- parsing some things a bit easier.
metaToJson :: MetaValue -> A.Value
metaToJson (MetaMap     m) = A.toJSON $! M.map metaToJson m
metaToJson (MetaList    l) = A.toJSON $! map metaToJson l
metaToJson (MetaBool    b) = A.toJSON b
metaToJson (MetaString  s) = A.toJSON s
metaToJson (MetaInlines i) =
    let !t = writeMarkdown def (Pandoc mempty [Plain i]) :: String in
    A.toJSON t
metaToJson (MetaBlocks  b) =
    let !t = writeMarkdown def (Pandoc mempty b) :: String in
    A.toJSON t
