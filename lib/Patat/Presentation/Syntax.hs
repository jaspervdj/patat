{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Patat.Presentation.Syntax
    ( Block (..)
    , Inline (..)

    , dftBlocks
    , dftInlines

    , fromPandocBlocks
    , fromPandocInlines

    , isHorizontalRule
    , isComment

    , Var (..)
    , variables

    , CounterID (..)
    , blocksTriggers
    , blocksApplyFragments
    , Counters
    , fragmentToBlocks
    , triggersToCounters

    , FragmentWrapper (..)
    , fragmentWrapper
    , Fragment (..)
    ) where

import           Control.Monad.Identity      (runIdentity)
import           Control.Monad.State         (State, execState, modify)
import           Control.Monad.Writer        (Writer, execWriter, tell)
import           Data.Hashable               (Hashable)
import qualified Data.HashSet                as HS
import           Data.List                   (foldl')
import qualified Data.Map                    as M
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Traversable            (for)
import qualified Data.Yaml                   as Yaml
import           Patat.Presentation.Settings (PresentationSettings)
import           Patat.Unique
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
    | Fragmented !FragmentWrapper !(Fragment [Block])
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
        Fragmented w fragments -> Fragmented w <$> traverse blocks fragments
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

fromPandocBlocks :: [Pandoc.Block] -> [Block]
fromPandocBlocks = concatMap fromPandocBlock

fromPandocBlock :: Pandoc.Block -> [Block]
fromPandocBlock (Pandoc.Plain xs) = [Plain (fromPandocInlines xs)]
fromPandocBlock (Pandoc.Para xs) = [Para (fromPandocInlines xs)]
fromPandocBlock (Pandoc.LineBlock xs) =
    [LineBlock (map fromPandocInlines xs)]
fromPandocBlock (Pandoc.CodeBlock attrs body) = [CodeBlock attrs body]
fromPandocBlock (Pandoc.RawBlock fmt body)
    -- Parse config blocks.
    | fmt == "html"
    , Just t1 <- T.stripPrefix "<!--config:" body
    , Just t2 <- T.stripSuffix "-->" t1 = pure $ Config $
        case Yaml.decodeEither' (T.encodeUtf8 t2) of
            Left err  -> Left (show err)
            Right obj -> Right obj
    -- Parse other comments.
    | Just t1 <- T.stripPrefix "<!--" body
    , Just t2 <- T.stripSuffix "-->" t1 = pure $ SpeakerNote $ T.strip t2
    -- Other raw blocks, leave as-is.
    | otherwise = [RawBlock fmt body]
fromPandocBlock (Pandoc.BlockQuote blocks) =
    [BlockQuote $ fromPandocBlocks blocks]
fromPandocBlock (Pandoc.OrderedList attrs items) =
    [OrderedList attrs $ map fromPandocBlocks items]
fromPandocBlock (Pandoc.BulletList items) =
    [BulletList $ map fromPandocBlocks items]
fromPandocBlock (Pandoc.DefinitionList items) = pure $ DefinitionList $ do
    (inlines, blockss) <- items
    pure (fromPandocInlines inlines, map (fromPandocBlocks) blockss)
fromPandocBlock (Pandoc.Header lvl attrs inlines) =
    [Header lvl attrs (fromPandocInlines inlines)]
fromPandocBlock Pandoc.HorizontalRule = [HorizontalRule]
fromPandocBlock (Pandoc.Table _ cptn specs thead tbodies tfoot) = pure $ Table
    (fromPandocInlines cptn')
    aligns
    (map (fromPandocBlocks) headers)
    (map (map fromPandocBlocks) rows)
  where
    (cptn', aligns, _, headers, rows) = Pandoc.toLegacyTable
        cptn specs thead tbodies tfoot

fromPandocBlock (Pandoc.Figure attrs _caption blocks) =
    [Figure attrs $ fromPandocBlocks blocks]
fromPandocBlock (Pandoc.Div attrs blocks) =
    [Div attrs $ fromPandocBlocks blocks]

fromPandocInlines :: [Pandoc.Inline] -> [Inline]
fromPandocInlines = concatMap fromPandocInline

fromPandocInline :: Pandoc.Inline -> [Inline]
fromPandocInline inline = case inline of
    Pandoc.Str txt           -> pure $ Str txt
    Pandoc.Emph        xs    -> pure $ Emph        (fromPandocInlines xs)
    Pandoc.Underline   xs    -> pure $ Underline   (fromPandocInlines xs)
    Pandoc.Strong      xs    -> pure $ Strong      (fromPandocInlines xs)
    Pandoc.Strikeout   xs    -> pure $ Strikeout   (fromPandocInlines xs)
    Pandoc.Superscript xs    -> pure $ Superscript (fromPandocInlines xs)
    Pandoc.Subscript   xs    -> pure $ Subscript   (fromPandocInlines xs)
    Pandoc.SmallCaps   xs    -> pure $ SmallCaps   (fromPandocInlines xs)
    Pandoc.Quoted ty   xs    -> pure $ Quoted ty   (fromPandocInlines xs)
    Pandoc.Cite c      xs    -> pure $ Cite c      (fromPandocInlines xs)
    Pandoc.Code attr txt     -> pure $ Code attr txt
    Pandoc.Space             -> pure $ Space
    Pandoc.SoftBreak         -> pure $ SoftBreak
    Pandoc.LineBreak         -> pure $ LineBreak
    Pandoc.Math ty txt       -> pure $ Math ty txt
    Pandoc.RawInline fmt txt -> pure $ RawInline fmt txt
    Pandoc.Link  attr xs tgt -> pure $ Link  attr (fromPandocInlines xs) tgt
    Pandoc.Image attr xs tgt -> pure $ Image attr (fromPandocInlines xs) tgt
    Pandoc.Note xs           -> pure $ Note (fromPandocBlocks xs)
    Pandoc.Span attr xs      -> pure $ Span attr (fromPandocInlines xs)

isHorizontalRule :: Block -> Bool
isHorizontalRule HorizontalRule = True
isHorizontalRule _              = False

isComment :: Block -> Bool
isComment (SpeakerNote _) = True
isComment (Config _)      = True
isComment _               = False

-- | A variable is like a placeholder in the instructions, something we don't
-- know yet, dynamic content.  Currently this is only used for code evaluation.
newtype Var = Var Unique deriving (Hashable, Eq, Ord, Show)

-- | Finds all variables that appear in some content.
variables :: [Block] -> HS.HashSet Var
variables = execWriter . dftBlocks visit (pure . pure)
  where
    visit :: Block -> Writer (HS.HashSet Var) [Block]
    visit b = do
        case b of
            VarBlock var -> tell $ HS.singleton var
            _            -> pure ()
        pure [b]

-- | A counter is used to change state in a slide.  As counters increment,
-- content may deterministically show or hide.
newtype CounterID = CounterID Unique deriving (Eq, Ord, Show)

-- | A fragment stores content which can be hidden or shown depending on
-- counter state.
--
-- The easiest example to think about is a bullet list which appears
-- incrmentally on a slide.  Initially, the counter state is 0.  As it is
-- incremented (the user goes to the next fragment in the slide), more list
-- items become visible.
data Fragment a = Fragment
    -- The ID of the counter for used this fragment.
    CounterID
    -- These counters should be fired in this order.
    -- Counter IDs will be included multiple times if needed.
    --
    -- This should only contain the ID of this counter, and IDs of counters
    -- nested inside the next field.
    [CounterID]
    -- For each piece of content in this fragment, we store a set of ints.
    -- When the current counter state is included in this set, the item is
    -- visible.
    [(S.Set Int, a)]
    deriving (Foldable, Functor, Eq, Show, Traversable)

-- | This determines how we construct content based on the visble items.
-- This could also be represented as `[[Block]] -> [Block]` but then we lose
-- the convenient Eq and Show instances.
data FragmentWrapper
    = ConcatWrapper
    | BulletListWrapper
    | OrderedListWrapper Pandoc.ListAttributes
    deriving (Eq, Show)

fragmentWrapper :: FragmentWrapper -> [[Block]] -> [Block]
fragmentWrapper ConcatWrapper             = concat
fragmentWrapper BulletListWrapper         = pure . BulletList
fragmentWrapper (OrderedListWrapper attr) = pure . OrderedList attr

-- | This does a deep traversal of some blocks, and returns all counters that
-- should be fired in-order.
blocksTriggers :: [Block] -> [CounterID]
blocksTriggers blocks = concat $
    execState (dftBlocks visit (pure . pure) blocks) []
  where
    -- We store a [[CounterID]] state, where each list represents the triggers
    -- necessary for a single fragmented block.
    visit :: Block -> State [[CounterID]] [Block]
    visit (Fragmented w fragment) = do
        modify $ merge fragment
        pure [Fragmented w fragment]
    visit block = pure [block]

    -- When we encounter a new fragmented block, we want to merge this into our
    -- [[CounterID]] state.  However, we need to ensure to remove any children
    -- of that fragmented block that were already in this list.
    merge :: Fragment [Block] -> [[CounterID]] -> [[CounterID]]
    merge (Fragment fid triggers _) known
        | any (fid `elem`) known = known
        | otherwise              =
            filter (not . any (`elem` triggers)) known ++ [triggers]

-- | Stores the state of several counters.
type Counters = M.Map CounterID Int

-- | Convert a list of counters that need to be triggered to the final state.
triggersToCounters :: [CounterID] -> Counters
triggersToCounters = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

-- | Render a fragment by applying its constructor to what is visible.
fragmentToBlocks :: Counters -> FragmentWrapper -> Fragment [Block] -> [Block]
fragmentToBlocks counters fw (Fragment cid _ sections) = fragmentWrapper fw
    [s | (activation, s) <- sections, counter `S.member` activation]
  where
    counter = fromMaybe 0 $ M.lookup cid counters

-- | Apply `fragmentToBlocks` recursively at each position, removing fragments
-- in favor of their currently visible content.
blocksApplyFragments :: Counters -> [Block] -> [Block]
blocksApplyFragments counters = runIdentity . dftBlocks visit (pure . pure)
  where
    visit (Fragmented w fragment) = pure $ fragmentToBlocks counters w fragment
    visit block                   = pure [block]
