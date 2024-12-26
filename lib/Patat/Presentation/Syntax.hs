module Patat.Presentation.Syntax
    ( Block (..)
    , foldBlock
    , foldInline

    , fromPandoc
    ) where

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
    | Figure !Pandoc.Attr ![Block]
    | Div !Pandoc.Attr ![Block]
    deriving (Eq, Show)

-- | Perform a deep fold over all inlines in a block.
-- Currently only used to find reference links.
foldBlock
    :: Monoid m
    => (Block -> m)
    -> (Pandoc.Inline -> m)
    -> Block -> m
foldBlock fb fi block = fb block <> case block of
    Plain is -> foldMap (foldInline fb fi) is
    Para is -> foldMap (foldInline fb fi) is
    LineBlock iss -> foldMap (foldMap (foldInline fb fi)) iss
    CodeBlock _ _ -> mempty
    RawBlock _ _ -> mempty
    BlockQuote bs -> foldMap (foldBlock fb fi) bs
    OrderedList _ bss -> foldMap (foldMap (foldBlock fb fi)) bss
    BulletList bss -> foldMap (foldMap (foldBlock fb fi)) bss
    DefinitionList items -> mconcat $ do
        (is, bss) <- items
        pure $ foldMap (foldInline fb fi) is <>
            foldMap (foldMap (foldBlock fb fi)) bss
    Header _ _ is -> foldMap (foldInline fb fi) is
    HorizontalRule -> mempty
    Table caption _ thead trows ->
        foldMap (foldInline fb fi) caption <>
        foldMap (foldMap (foldBlock fb fi)) thead <>
        foldMap (foldMap (foldMap (foldBlock fb fi))) trows
    Figure _ bs -> foldMap (foldBlock fb fi) bs
    Div _ bs -> foldMap (foldBlock fb fi) bs

foldInline
    :: Monoid m
    => (Block -> m)
    -> (Pandoc.Inline -> m)
    -> Pandoc.Inline -> m
foldInline fb fi inline = fi inline <> case inline of
    Pandoc.Str _ -> mempty
    Pandoc.Emph is -> foldMap (foldInline fb fi) is
    Pandoc.Underline is -> foldMap (foldInline fb fi) is
    Pandoc.Strong is -> foldMap (foldInline fb fi) is
    Pandoc.Strikeout is -> foldMap (foldInline fb fi) is
    Pandoc.Superscript is -> foldMap (foldInline fb fi) is
    Pandoc.Subscript is -> foldMap (foldInline fb fi) is
    Pandoc.SmallCaps is -> foldMap (foldInline fb fi) is
    Pandoc.Quoted _ is -> foldMap (foldInline fb fi) is
    Pandoc.Cite _ is -> foldMap (foldInline fb fi) is
    Pandoc.Code _ _ -> mempty
    Pandoc.Space -> mempty
    Pandoc.SoftBreak -> mempty
    Pandoc.LineBreak -> mempty
    Pandoc.Math _ _ -> mempty
    Pandoc.RawInline _ _ -> mempty
    Pandoc.Link _ is _ -> foldMap (foldInline fb fi) is
    Pandoc.Image _ is _ -> foldMap (foldInline fb fi) is
    Pandoc.Note bs ->
        -- TODO: this should use our block type...
        -- foldMap (foldBlock fb fi) bs
        undefined
    Pandoc.Span _ is -> foldMap (foldInline fb fi) is

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
