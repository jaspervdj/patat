{-# LANGUAGE DeriveDataTypeable #-}
module Patat.Presentation.Syntax
    ( Block (..)
    , fromPandoc
    ) where

import           Data.Data                  (Data)
import qualified Data.Text                  as T
import qualified Text.Pandoc                as Pandoc
import qualified Text.Pandoc.Writers.Shared as Pandoc

-- | This is similar to 'Pandoc.Block'.  Having our own datatype has some
-- advantages:
--
-- * We can extend it with slide-specific data (eval, fragments)
-- * We can remove stuff we don't care about
-- * We can parse attributes and move them to haskell datatypes
-- * This conversion can happen in a single parsing phase
-- * We can catch backwards-incompatible pandoc changes in this module
--
-- We try to follow the naming conventions from Pandoc as much as possible.
--
-- At some point we could do the same for 'Pandoc.Inline', but that doesn't
-- really have similar advantages right now.
data Block
    = Plain ![Pandoc.Inline]
    | Para ![Pandoc.Inline]
    | LineBlock ![[Pandoc.Inline]]
    | CodeBlock !Pandoc.Attr !T.Text
    | RawBlock !Pandoc.Format !T.Text
    | BlockQuote ![Block]
    | OrderedList !Pandoc.ListAttributes ![[Block]]
    | BulletList ![[Block]]
    | DefinitionList ![([Pandoc.Inline], [[Block]])]
    | Header Int !Pandoc.Attr ![Pandoc.Inline]
    | HorizontalRule
    | Table ![Pandoc.Inline] ![Pandoc.Alignment] ![[Block]] ![[[Block]]]
    | Figure !Pandoc.Attr !Pandoc.Caption ![Block]
    | Div !Pandoc.Attr ![Block]
    -- TODO: remove Data instance
    deriving (Eq, Data, Show)

fromPandoc :: Pandoc.Block -> Block
fromPandoc (Pandoc.Plain inlines) = Plain inlines
fromPandoc (Pandoc.Para inlines) = Para inlines
fromPandoc (Pandoc.LineBlock inliness) = LineBlock inliness
fromPandoc (Pandoc.CodeBlock attrs body) = CodeBlock attrs body
fromPandoc (Pandoc.RawBlock fmt body) = RawBlock fmt body
fromPandoc (Pandoc.BlockQuote blocks) =
    BlockQuote $ map fromPandoc blocks
fromPandoc (Pandoc.OrderedList attrs items) =
    OrderedList attrs $ map (map fromPandoc) items
fromPandoc (Pandoc.BulletList items) =
    BulletList $ map (map fromPandoc) items
fromPandoc (Pandoc.DefinitionList items) = DefinitionList $ do
    (inlines, blockss) <- items
    pure (inlines, map (map fromPandoc) blockss)
fromPandoc (Pandoc.Header lvl attrs inlines) = Header lvl attrs inlines
fromPandoc Pandoc.HorizontalRule = HorizontalRule
fromPandoc (Pandoc.Table _ caption specs thead tbodies tfoot) = Table
    caption'
    aligns
    (map (map fromPandoc) headers)
    (map (map (map fromPandoc)) rows)
  where
    (caption', aligns, _, headers, rows) = Pandoc.toLegacyTable
        caption specs thead tbodies tfoot

fromPandoc (Pandoc.Figure attrs caption blocks) =
    Figure attrs caption $ map fromPandoc blocks
fromPandoc (Pandoc.Div attrs blocks) =
    Div attrs $ map fromPandoc blocks
