--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase #-}
module Text.Pandoc.Extended
    ( module Text.Pandoc
    , readPlainText
    ) where


--------------------------------------------------------------------------------
import           Data.Char   (isSpace)
import qualified Data.Text   as T
import           Prelude
import           Text.Pandoc


--------------------------------------------------------------------------------
-- | A plain-text reader.  Always returns empty metadata.
readPlainText :: T.Text -> Pandoc
readPlainText = Pandoc mempty . pure . Plain . go
  where
    go txt0 = case T.uncons txt0 of
        Nothing           -> []
        Just (' ',  txt1) -> Space : go txt1
        Just ('\r', txt1) -> go txt1
        Just ('\n', txt1) -> SoftBreak : go txt1
        Just (c,    txt1) ->
            let (pre, post) = T.break isSpace txt1 in
            Str (T.cons c pre) : go post
