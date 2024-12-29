{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Patat.Presentation.Syntax
    ( Var
    , VarGen
    , zeroVarGen
    , freshVar

    , Block (..)
    , Inline (..)

    , dftBlocks
    , dftInlines

    , fromPandocBlock
    , fromPandocInline

    , isHorizontalRule
    , isComment

    , variables
    ) where

import           Control.Monad.Writer        (Writer, execWriter, tell)
import           Data.Hashable               (Hashable)
import qualified Data.HashSet                as HS
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Traversable            (for)
import qualified Data.Yaml                   as Yaml
import           Patat.Presentation.Settings (PresentationSettings)
import qualified Text.Pandoc                 as Pandoc
import qualified Text.Pandoc.Writers.Shared  as Pandoc

-- | A variable is like a placeholder in the instructions, something we don't
-- know yet, dynamic content.  Currently this is only used for code evaluation.
newtype Var = Var Int deriving (Hashable, Eq, Ord, Show)

-- | Used to generate fresh variables.
newtype VarGen = VarGen Int deriving (Show)

zeroVarGen :: VarGen
zeroVarGen = VarGen 0

freshVar :: VarGen -> (Var, VarGen)
freshVar (VarGen x) = (Var x, VarGen (x + 1))

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
    | VarBlock !Var
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
dftBlocks
    :: forall m. Monad m
    => (Block -> m [Block])
    -> (Inline -> m [Inline])
    -> [Block] -> m [Block]
dftBlocks fb fi = blocks
  where
    blocks :: [Block] -> m [Block]
    blocks = fmap concat . traverse block

    inlines :: [Inline] -> m [Inline]
    inlines = dftInlines fb fi

    block :: Block -> m [Block]
    block = (>>= fb) . \case
        Plain xs -> Plain <$> inlines xs
        Para xs -> Para <$> inlines xs
        LineBlock xss -> LineBlock <$> traverse inlines xss
        b@(CodeBlock _attr _txt) -> pure b
        b@(RawBlock _fmt _txt) -> pure b
        BlockQuote xs -> BlockQuote <$> blocks xs
        OrderedList attr xss -> OrderedList attr <$> traverse blocks xss
        BulletList xss ->BulletList <$> traverse blocks xss
        DefinitionList xss -> DefinitionList <$> for xss
            (\(term, definition) -> (,)
                <$> inlines term
                <*> traverse blocks definition)
        Header lvl attr xs -> Header lvl attr <$> inlines xs
        b@HorizontalRule -> pure b
        Table cptn aligns thead trows -> Table
            <$> inlines cptn
            <*> pure aligns
            <*> traverse blocks thead
            <*> traverse (traverse blocks) trows
        Figure attr xs -> Figure attr <$> blocks xs
        Div attr xs -> Div attr <$> blocks xs
        b@(VarBlock _var) -> pure b
        b@(SpeakerNote _txt) -> pure b
        b@(Config _cfg) -> pure b

-- | Depth-First Traversal of inlines (and blocks).
dftInlines
    :: forall m. Monad m
    => (Block -> m [Block])
    -> (Inline -> m [Inline])
    -> [Inline] -> m [Inline]
dftInlines fb fi = inlines
  where
    inlines :: [Inline] -> m [Inline]
    inlines = fmap concat . traverse inline

    inline :: Inline -> m [Inline]
    inline = (>>= fi) . \case
        i@(Str _txt) -> pure i
        Emph        xs -> Emph        <$> inlines xs
        Underline   xs -> Underline   <$> inlines xs
        Strong      xs -> Strong      <$> inlines xs
        Strikeout   xs -> Strikeout   <$> inlines xs
        Superscript xs -> Superscript <$> inlines xs
        Subscript   xs -> Subscript   <$> inlines xs
        SmallCaps   xs -> SmallCaps   <$> inlines xs
        Quoted ty   xs -> Quoted ty   <$> inlines xs
        Cite c      xs -> Cite c      <$> inlines xs
        i@(Code _attr _txt)     -> pure i
        i@Space                 -> pure i
        i@SoftBreak             -> pure i
        i@LineBreak             -> pure i
        i@(Math _ty _txt)       -> pure i
        i@(RawInline _fmt _txt) -> pure i
        Link  attr xs tgt -> Link  attr <$> inlines xs <*> pure tgt
        Image attr xs tgt -> Image attr <$> inlines xs <*> pure tgt
        Note blocks -> Note <$> dftBlocks fb fi blocks
        Span attr xs -> Span attr . concat <$> traverse inline xs

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
    Pandoc.Str txt           -> Str txt
    Pandoc.Emph        xs    -> Emph        (map fromPandocInline xs)
    Pandoc.Underline   xs    -> Underline   (map fromPandocInline xs)
    Pandoc.Strong      xs    -> Strong      (map fromPandocInline xs)
    Pandoc.Strikeout   xs    -> Strikeout   (map fromPandocInline xs)
    Pandoc.Superscript xs    -> Superscript (map fromPandocInline xs)
    Pandoc.Subscript   xs    -> Subscript   (map fromPandocInline xs)
    Pandoc.SmallCaps   xs    -> SmallCaps   (map fromPandocInline xs)
    Pandoc.Quoted ty   xs    -> Quoted ty   (map fromPandocInline xs)
    Pandoc.Cite c      xs    -> Cite c      (map fromPandocInline xs)
    Pandoc.Code attr txt     -> Code attr txt
    Pandoc.Space             -> Space
    Pandoc.SoftBreak         -> SoftBreak
    Pandoc.LineBreak         -> LineBreak
    Pandoc.Math ty txt       -> Math ty txt
    Pandoc.RawInline fmt txt -> RawInline fmt txt
    Pandoc.Link  attr xs tgt -> Link  attr (map fromPandocInline xs) tgt
    Pandoc.Image attr xs tgt -> Image attr (map fromPandocInline xs) tgt
    Pandoc.Note xs           -> Note (map fromPandocBlock xs)
    Pandoc.Span attr xs      -> Span attr (map fromPandocInline xs)

isHorizontalRule :: Block -> Bool
isHorizontalRule HorizontalRule = True
isHorizontalRule _              = False

isComment :: Block -> Bool
isComment (SpeakerNote _) = True
isComment (Config _)      = True
isComment _               = False

variables :: [Block] -> HS.HashSet Var
variables = execWriter . dftBlocks visit (pure . pure)
  where
    visit :: Block -> Writer (HS.HashSet Var) [Block]
    visit b = do
        case b of
            VarBlock var -> tell $ HS.singleton var
            _            -> pure ()
        pure [b]
