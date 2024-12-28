{-# LANGUAGE LambdaCase #-}
module Patat.Presentation.Syntax
    ( Block (..)

    , dftBlock
    , dftInline

    , fromPandoc
    ) where

import qualified Data.Text                  as T
import           Data.Traversable           (for)
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
    | Figure !Pandoc.Attr ![Block]
    | Div !Pandoc.Attr ![Block]
    deriving (Eq, Show)

-- | Depth-First Traversal of blocks (and inlines).
dftBlock
    :: Monad m
    => (Block -> m Block)
    -> (Pandoc.Inline -> m Pandoc.Inline)
    -> Block -> m Block
dftBlock fb fi = (>>= fb) . \case
    Plain xs -> Plain <$> traverse inline xs
    Para xs -> Para <$> traverse inline xs
    LineBlock xss -> LineBlock <$> traverse (traverse inline) xss
    b@(CodeBlock _attr _txt) -> pure b
    b@(RawBlock _fmt _txt) -> pure b
    BlockQuote xs -> BlockQuote <$> traverse block xs
    OrderedList attr xss ->
        OrderedList attr <$> traverse (traverse block) xss
    BulletList xss ->BulletList <$> traverse (traverse block) xss
    DefinitionList xss -> DefinitionList <$> for xss
        (\(term, definition) -> (,)
            <$> traverse inline term
            <*> traverse (traverse block) definition)
    Header lvl attr xs -> Header lvl attr <$> traverse inline xs
    b@HorizontalRule -> pure b
    Table cptn aligns thead trows -> Table
        <$> traverse inline cptn
        <*> pure aligns
        <*> traverse (traverse block) thead
        <*> traverse (traverse (traverse block)) trows
    Figure attr xs -> Figure attr <$> traverse block xs
    Div attr xs -> Div attr <$> traverse block xs
  where
    block  = dftBlock fb fi
    inline = dftInline fb fi

-- | Depth-First Traversal of inlines (and blocks).
dftInline
    :: Monad m
    => (Block -> m Block)
    -> (Pandoc.Inline -> m Pandoc.Inline)
    -> Pandoc.Inline -> m Pandoc.Inline
dftInline fb fi = (>>= fi) . \case
    i@(Pandoc.Str _txt) -> pure i
    Pandoc.Emph        xs -> Pandoc.Emph        <$> traverse inline xs
    Pandoc.Underline   xs -> Pandoc.Underline   <$> traverse inline xs
    Pandoc.Strong      xs -> Pandoc.Strong      <$> traverse inline xs
    Pandoc.Strikeout   xs -> Pandoc.Strikeout   <$> traverse inline xs
    Pandoc.Superscript xs -> Pandoc.Superscript <$> traverse inline xs
    Pandoc.Subscript   xs -> Pandoc.Subscript   <$> traverse inline xs
    Pandoc.SmallCaps   xs -> Pandoc.SmallCaps   <$> traverse inline xs
    Pandoc.Quoted ty   xs -> Pandoc.Quoted ty   <$> traverse inline xs
    Pandoc.Cite c      xs -> Pandoc.Cite c      <$> traverse inline xs
    i@(Pandoc.Code _attr _txt)     -> pure i
    i@Pandoc.Space                 -> pure i
    i@Pandoc.SoftBreak             -> pure i
    i@Pandoc.LineBreak             -> pure i
    i@(Pandoc.Math _ty _txt)       -> pure i
    i@(Pandoc.RawInline _fmt _txt) -> pure i
    Pandoc.Link attr xs tgt ->
        Pandoc.Link attr <$> traverse inline xs <*> pure tgt
    Pandoc.Image attr xs tgt ->
        Pandoc.Image attr <$> traverse inline xs <*> pure tgt
    -- TODO: This is broken because we don't define our own Inline type using
    -- our own Block.   It's probably fine since Note is pretty much unused.
    i@(Pandoc.Note _blocks) -> pure i
    Pandoc.Span attr xs -> Pandoc.Span attr <$> traverse inline xs
  where
    inline = dftInline fb fi

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

fromPandoc (Pandoc.Figure attrs _caption blocks) =
    Figure attrs $ map fromPandoc blocks
fromPandoc (Pandoc.Div attrs blocks) =
    Div attrs $ map fromPandoc blocks
