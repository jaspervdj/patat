{-# LANGUAGE LambdaCase #-}
module Text.Pandoc.Extended
    ( module Text.Pandoc

    , plainToPara
    ) where

import Text.Pandoc

plainToPara :: [Block] -> [Block]
plainToPara = map $ \case
    Plain inlines -> Para inlines
    block         -> block
