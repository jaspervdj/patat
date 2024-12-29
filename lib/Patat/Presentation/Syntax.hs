{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Patat.Presentation.Syntax
    ( Block (..)
    , Inline (..)

    , dftBlock
    , dftInline

    , fromPandocBlock
    , fromPandocInline

    , isHorizontalRule
    , isComment
    ) where

import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Traversable            (for)
import qualified Data.Yaml                   as Yaml
import           Patat.Presentation.Settings (PresentationSettings)
import qualified Text.Pandoc                 as Pandoc
import qualified Text.Pandoc.Writers.Shared  as Pandoc

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
data Block
    = Plain ![Inline]
    | Para ![Inline]
    | LineBlock ![[Inline]]
    | CodeBlock !Pandoc.Attr !T.Text
    | RawBlock !Pandoc.Format !T.Text
    | BlockQuote ![Block]
    | OrderedList !Pandoc.ListAttributes ![[Block]]
    | BulletList ![[Block]]
    | DefinitionList ![([Inline], [[Block]])]
    | Header Int !Pandoc.Attr ![Inline]
    | HorizontalRule
    | Table ![Inline] ![Pandoc.Alignment] ![[Block]] ![[[Block]]]
    | Figure !Pandoc.Attr ![Block]
    | Div !Pandoc.Attr ![Block]
    -- Our own extensions:
    | SpeakerNote !T.Text
    | Config !(Either String PresentationSettings)
    deriving (Eq, Show)

-- | See comment on 'Block'.
data Inline
    = Str !T.Text
    | Emph ![Inline]
    | Underline ![Inline]
    | Strong ![Inline]
    | Strikeout ![Inline]
    | Superscript ![Inline]
    | Subscript ![Inline]
    | SmallCaps ![Inline]
    | Quoted !Pandoc.QuoteType ![Inline]
    | Cite ![Pandoc.Citation] ![Inline]
    | Code !Pandoc.Attr !T.Text
    | Space
    | SoftBreak
    | LineBreak
    | Math !Pandoc.MathType !T.Text
    | RawInline !Pandoc.Format !T.Text
    | Link !Pandoc.Attr ![Inline] !Pandoc.Target
    | Image !Pandoc.Attr ![Inline] !Pandoc.Target
    | Note ![Block]
    | Span !Pandoc.Attr ![Inline]
    deriving (Eq, Show)

-- | Depth-First Traversal of blocks (and inlines).
dftBlock
    :: Monad m
    => (Block -> m Block)
    -> (Inline -> m Inline)
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
    b@(SpeakerNote _txt) -> pure b
    b@(Config _cfg) -> pure b
  where
    block  = dftBlock fb fi
    inline = dftInline fb fi

-- | Depth-First Traversal of inlines (and blocks).
dftInline
    :: Monad m
    => (Block -> m Block)
    -> (Inline -> m Inline)
    -> Inline -> m Inline
dftInline fb fi = (>>= fi) . \case
    i@(Str _txt) -> pure i
    Emph        xs -> Emph        <$> traverse inline xs
    Underline   xs -> Underline   <$> traverse inline xs
    Strong      xs -> Strong      <$> traverse inline xs
    Strikeout   xs -> Strikeout   <$> traverse inline xs
    Superscript xs -> Superscript <$> traverse inline xs
    Subscript   xs -> Subscript   <$> traverse inline xs
    SmallCaps   xs -> SmallCaps   <$> traverse inline xs
    Quoted ty   xs -> Quoted ty   <$> traverse inline xs
    Cite c      xs -> Cite c      <$> traverse inline xs
    i@(Code _attr _txt)     -> pure i
    i@Space                 -> pure i
    i@SoftBreak             -> pure i
    i@LineBreak             -> pure i
    i@(Math _ty _txt)       -> pure i
    i@(RawInline _fmt _txt) -> pure i
    Link  attr xs tgt -> Link  attr <$> traverse inline xs <*> pure tgt
    Image attr xs tgt -> Image attr <$> traverse inline xs <*> pure tgt
    Note blocks -> Note <$> traverse (dftBlock fb fi) blocks
    Span attr xs -> Span attr <$> traverse inline xs
  where
    inline = dftInline fb fi

fromPandocBlock :: Pandoc.Block -> Block
fromPandocBlock (Pandoc.Plain xs) = Plain (map fromPandocInline xs)
fromPandocBlock (Pandoc.Para xs) = Para (map fromPandocInline xs)
fromPandocBlock (Pandoc.LineBlock xs) =
    LineBlock (map (map fromPandocInline) xs)
fromPandocBlock (Pandoc.CodeBlock attrs body) = CodeBlock attrs body
fromPandocBlock (Pandoc.RawBlock fmt body)
    -- Parse config blocks.
    | fmt == "html"
    , Just t1 <- T.stripPrefix "<!--config:" body
    , Just t2 <- T.stripSuffix "-->" t1 = Config $
        case Yaml.decodeEither' (T.encodeUtf8 t2) of
            Left err  -> Left (show err)
            Right obj -> Right obj
    -- Parse other comments.
    | Just t1 <- T.stripPrefix "<!--" body
    , Just t2 <- T.stripSuffix "-->" t1 = SpeakerNote $ T.strip t2
    -- Other raw blocks, leave as-is.
    | otherwise = RawBlock fmt body
fromPandocBlock (Pandoc.BlockQuote blocks) =
    BlockQuote $ map fromPandocBlock blocks
fromPandocBlock (Pandoc.OrderedList attrs items) =
    OrderedList attrs $ map (map fromPandocBlock) items
fromPandocBlock (Pandoc.BulletList items) =
    BulletList $ map (map fromPandocBlock) items
fromPandocBlock (Pandoc.DefinitionList items) = DefinitionList $ do
    (inlines, blockss) <- items
    pure (map fromPandocInline inlines, map (map fromPandocBlock) blockss)
fromPandocBlock (Pandoc.Header lvl attrs inlines) =
    Header lvl attrs (map fromPandocInline inlines)
fromPandocBlock Pandoc.HorizontalRule = HorizontalRule
fromPandocBlock (Pandoc.Table _ caption specs thead tbodies tfoot) = Table
    (map fromPandocInline caption')
    aligns
    (map (map fromPandocBlock) headers)
    (map (map (map fromPandocBlock)) rows)
  where
    (caption', aligns, _, headers, rows) = Pandoc.toLegacyTable
        caption specs thead tbodies tfoot

fromPandocBlock (Pandoc.Figure attrs _caption blocks) =
    Figure attrs $ map fromPandocBlock blocks
fromPandocBlock (Pandoc.Div attrs blocks) =
    Div attrs $ map fromPandocBlock blocks

fromPandocInline :: Pandoc.Inline -> Inline
fromPandocInline inline = case inline of
    Pandoc.Str txt -> Str txt
    Pandoc.Emph        xs -> Emph        (map fromPandocInline xs)
    Pandoc.Underline   xs -> Underline   (map fromPandocInline xs)
    Pandoc.Strong      xs -> Strong      (map fromPandocInline xs)
    Pandoc.Strikeout   xs -> Strikeout   (map fromPandocInline xs)
    Pandoc.Superscript xs -> Superscript (map fromPandocInline xs)
    Pandoc.Subscript   xs -> Subscript   (map fromPandocInline xs)
    Pandoc.SmallCaps   xs -> SmallCaps   (map fromPandocInline xs)
    Pandoc.Quoted ty   xs -> Quoted ty   (map fromPandocInline xs)
    Pandoc.Cite c      xs -> Cite c      (map fromPandocInline xs)
    Pandoc.Code attr txt -> Code attr txt
    Pandoc.Space     -> Space
    Pandoc.SoftBreak -> SoftBreak
    Pandoc.LineBreak -> LineBreak
    Pandoc.Math ty txt -> Math ty txt
    Pandoc.RawInline fmt txt -> RawInline fmt txt
    Pandoc.Link  attr xs tgt -> Link  attr (map fromPandocInline xs) tgt
    Pandoc.Image attr xs tgt -> Image attr (map fromPandocInline xs) tgt
    Pandoc.Note xs -> Note (map fromPandocBlock xs)
    Pandoc.Span attr xs -> Span attr (map fromPandocInline xs)

isHorizontalRule :: Block -> Bool
isHorizontalRule HorizontalRule = True
isHorizontalRule _              = False

isComment :: Block -> Bool
isComment (SpeakerNote _) = True
isComment (Config _)      = True
isComment _               = False
