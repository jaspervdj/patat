--------------------------------------------------------------------------------
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Presentation.Display
    ( Display (..)
    , displayPresentation
    , displayPresentationError
    , dumpPresentation
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                        (guard)
import           Control.Monad.Identity               (runIdentity)
import           Control.Monad.Writer                 (Writer, execWriter, tell)
import qualified Data.Aeson.Extended                  as A
import           Data.Char.WCWidth.Extended           (wcstrwidth)
import           Data.Foldable                        (for_)
import qualified Data.HashMap.Strict                  as HMS
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe, maybeToList)
import qualified Data.Sequence.Extended               as Seq
import qualified Data.Text                            as T
import           Patat.Presentation.Display.CodeBlock
import           Patat.Presentation.Display.Internal
import           Patat.Presentation.Display.Table
import           Patat.Presentation.Internal
import           Patat.Presentation.Settings
import qualified Patat.Presentation.SpeakerNotes      as SpeakerNotes
import           Patat.Presentation.Syntax
import           Patat.PrettyPrint                    ((<$$>), (<+>))
import qualified Patat.PrettyPrint                    as PP
import           Patat.Size
import           Patat.Theme                          (Theme (..))
import qualified Patat.Theme                          as Theme
import           Prelude
import qualified Text.Pandoc.Extended                 as Pandoc


--------------------------------------------------------------------------------
data Display = DisplayDoc PP.Doc | DisplayImage FilePath deriving (Show)


--------------------------------------------------------------------------------
-- | Display something within the presentation borders that draw the title and
-- the active slide number and so on.
displayWithBorders
    :: Size -> Presentation -> (DisplaySettings -> PP.Doc) -> PP.Doc
displayWithBorders (Size rows columns) pres@Presentation {..} f =
    (if null title
        then mempty
        else
            let titleRemainder = columns - titleWidth - titleOffset
                wrappedTitle = PP.spaces titleOffset <> PP.string title <> PP.spaces titleRemainder in
        borders wrappedTitle <> PP.hardline) <>
    f ds <> PP.hardline <>
    PP.goToLine (rows - 2) <>
    borders (PP.space <> PP.string author <> middleSpaces <> PP.string active <> PP.space) <>
    PP.hardline
  where
    -- Get terminal width/title
    settings     = activeSettings pres
    (sidx, _)    = pActiveFragment
    ds           = DisplaySettings
        { dsSize          = canvasSize
        , dsMargins       = margins settings
        , dsWrap          = fromMaybe NoWrap $ psWrap settings
        , dsTabStop       = maybe 4 A.unFlexibleNum $ psTabStop settings
        , dsTheme         = fromMaybe Theme.defaultTheme (psTheme settings)
        , dsSyntaxMap     = pSyntaxMap
        , dsResolve       = \var -> fromMaybe [] $ HMS.lookup var pVars
        , dsRevealState   = revealState
        }

    revealState = case activeFragment pres of
        Just (ActiveContent _ _ c) -> c
        _                          -> mempty

    -- Compute title.
    breadcrumbs = fromMaybe [] $ Seq.safeIndex pBreadcrumbs sidx
    plainTitle  = PP.toString $ prettyInlines ds pTitle
    breadTitle  = mappend plainTitle $ mconcat
        [ s
        | b <- map (prettyInlines ds . snd) breadcrumbs
        , s <- [" > ", PP.toString b]
        ]
    title
        | not . fromMaybe True $ psBreadcrumbs settings = plainTitle
        | wcstrwidth breadTitle > columns               = plainTitle
        | otherwise                                     = breadTitle

    -- Dimensions of title.
    titleWidth  = wcstrwidth title
    titleOffset = (columns - titleWidth) `div` 2
    borders     = themed ds themeBorders

    -- Room left for content
    canvasSize = Size (rows - 3) columns

    -- Compute footer.
    active
        | fromMaybe True $ psSlideNumber settings = show (sidx + 1) ++ " / " ++ show (length pSlides)
        | otherwise                               = ""
    activeWidth  = wcstrwidth active
    author       = PP.toString (prettyInlines ds pAuthor)
    authorWidth  = wcstrwidth author
    middleSpaces = PP.spaces $ columns - activeWidth - authorWidth - 2


--------------------------------------------------------------------------------
displayPresentation :: Size -> Presentation -> Display
displayPresentation size pres@Presentation {..} =
     case activeFragment pres of
        Nothing -> DisplayDoc $ displayWithBorders size pres mempty
        Just (ActiveContent fragment _ _)
                | Just _ <- psImages pSettings
                , Just image <- onlyImage fragment ->
            DisplayImage $ T.unpack image
        Just (ActiveContent fragment _ _) -> DisplayDoc $
            displayWithBorders size pres $ \theme ->
                prettyMargins theme fragment
        Just (ActiveTitle block) -> DisplayDoc $
            displayWithBorders size pres $ \ds ->
                let auto = Margins {mTop = Auto, mRight = Auto, mLeft = Auto} in
                prettyMargins ds {dsMargins = auto} [block]
  where
    -- Check if the fragment consists of "just a single image".  Discard
    -- headers.
    onlyImage (Header{} : bs) = onlyImage bs
    onlyImage bs = case bs of
        [Figure _ bs']                 -> onlyImage bs'
        [Para [Image _ _ (target, _)]] -> Just target
        _                              -> Nothing


--------------------------------------------------------------------------------
-- | Displays an error in the place of the presentation.  This is useful if we
-- want to display an error but keep the presentation running.
displayPresentationError :: Size -> Presentation -> String -> PP.Doc
displayPresentationError size pres err = displayWithBorders size pres $ \ds ->
    themed ds themeStrong "Error occurred in the presentation:" <$$>
    "" <$$>
    (PP.string err)


--------------------------------------------------------------------------------
dumpPresentation :: Presentation -> IO ()
dumpPresentation pres@Presentation {..} =
    PP.putDoc $ PP.removeControls $
    PP.vcat $ L.intercalate ["{slide}"] $
        map dumpSlide [0 .. length pSlides - 1]
  where
    dumpSlide :: Int -> [PP.Doc]
    dumpSlide i = do
        slide <- maybeToList $ getSlide i pres
        dumpSpeakerNotes slide <> L.intercalate ["{fragment}"]
            [ dumpFragment (i, j)
            | j <- [0 .. numFragments slide - 1]
            ]

    dumpSpeakerNotes :: Slide -> [PP.Doc]
    dumpSpeakerNotes slide = do
        guard (slideSpeakerNotes slide /= mempty)
        pure $ PP.text $ "{speakerNotes: " <>
            SpeakerNotes.toText (slideSpeakerNotes slide) <> "}"

    dumpFragment :: Index -> [PP.Doc]
    dumpFragment idx =
        case displayPresentation (getSize idx) pres {pActiveFragment = idx} of
            DisplayDoc   doc      -> [doc]
            DisplayImage filepath -> [PP.string $ "{image: " ++ filepath ++ "}"]

    getSize :: Index -> Size
    getSize idx =
        let settings = activeSettings pres {pActiveFragment = idx}
            sRows    = fromMaybe 24 $ A.unFlexibleNum <$> psRows settings
            sCols    = fromMaybe 72 $ A.unFlexibleNum <$> psColumns settings in
        Size {..}


--------------------------------------------------------------------------------
-- | Renders the given blocks, adding margins based on the settings and wrapping
-- based on width.
prettyMargins :: DisplaySettings -> [Block] -> PP.Doc
prettyMargins ds blocks = vertical $
    map horizontal blocks ++
    case prettyReferences ds blocks of
        []   -> []
        refs ->
            let doc0        = PP.vcat refs
                size@(r, _) = PP.dimensions doc0 in
            [(horizontalIndent size $ horizontalWrap doc0, r)]
  where
    Size rows columns = dsSize ds
    Margins {..} = dsMargins ds

    -- For every block, calculate the size based on its last fragment.
    blockSize block =
        let revealState = blocksRevealLastStep [block] in
        PP.dimensions $ deindent $ horizontalWrap $
            prettyBlock ds {dsRevealState = revealState} block

    -- Vertically align some blocks by adding spaces in front of it.
    -- We also take in the number of rows for every block so we don't
    -- need to recompute it.
    vertical :: [(PP.Doc, Int)] -> PP.Doc
    vertical docs0 = mconcat (replicate top PP.hardline) <> doc
      where
        top = case mTop of
            Auto      -> (rows - actual) `div` 2
            NotAuto x -> x

        docs1  = [verticalPad r d | (d, r) <- docs0]
        actual = sum $ L.intersperse 1 $ map snd docs1
        doc    = PP.vcat $ map fst docs1

    -- Vertically pad a doc by adding lines below it.
    -- Return the actual size as well as the padded doc.
    verticalPad :: Int -> PP.Doc -> (PP.Doc, Int)
    verticalPad desired doc0
        | actual >= rows = (doc0, actual)
        | otherwise      = (doc0 <> padding, desired)
      where
        (actual, _) = PP.dimensions doc0
        padding     = mconcat $ replicate (desired - actual) PP.hardline

    -- Render and horizontally align a block.  Also returns the desired rows.
    horizontal :: Block -> (PP.Doc, Int)
    horizontal b@(Reveal ConcatWrapper reveal) =
        -- Horizontally aligning a fragment with a ConcatWrapper is a special
        -- case, as we want to horizontal align all the things inside
        -- individually.
        let (fblocks, _) = unzip $ map horizontal $
                revealToBlocks (dsRevealState ds) ConcatWrapper reveal in
        (PP.vcat fblocks, fst (blockSize b))
    horizontal block =
        let size@(r, _) = blockSize block in
        (horizontalIndent size $ horizontalWrap $ prettyBlock ds block, r)

    horizontalIndent :: (Int, Int) -> PP.Doc -> PP.Doc
    horizontalIndent (_, dcols) doc0 = PP.indent indentation indentation doc1
      where
        doc1 = deindent doc0
        left = case mLeft of
            NotAuto x -> x
            Auto      -> case mRight of
                NotAuto _ -> 0
                Auto      -> (columns - dcols) `div` 2
        indentation = PP.Indentation left mempty

    -- Strip leading spaces to horizontally align code blocks etc.
    deindent doc0 = case (mLeft, mRight) of
        (Auto, Auto) -> PP.deindent doc0
        _            -> doc0

    -- Rearranges lines to fit into the wrap settings.
    horizontalWrap :: PP.Doc -> PP.Doc
    horizontalWrap doc0 = case dsWrap ds of
        NoWrap     -> doc0
        AutoWrap   -> PP.wrapAt (Just $ columns - right - left) doc0
        WrapAt col -> PP.wrapAt (Just col) doc0
      where
        right = case mRight of
            Auto      -> 0
            NotAuto x -> x
        left = case mLeft of
            Auto      -> 0
            NotAuto x -> x


--------------------------------------------------------------------------------
prettyBlock :: DisplaySettings -> Block -> PP.Doc

prettyBlock ds (Plain inlines) = prettyInlines ds inlines

prettyBlock ds (Para inlines) =
    prettyInlines ds inlines <> PP.hardline

prettyBlock ds (Header i _ inlines) =
    themed ds themeHeader (PP.string (replicate i '#') <+> prettyInlines ds inlines) <>
    PP.hardline

prettyBlock ds (CodeBlock (_, classes, _) txt) =
    prettyCodeBlock ds classes txt

prettyBlock ds (BulletList bss) = PP.vcat
    [ PP.indent
        (PP.Indentation 2 $ themed ds themeBulletList prefix)
        (PP.Indentation 4 mempty)
        (prettyBlocks ds' bs)
    | bs <- bss
    ] <> PP.hardline
  where
    prefix = PP.string [marker] <> " "
    marker = case T.unpack <$> themeBulletListMarkers theme of
        Just (x : _) -> x
        _            -> '-'

    -- Cycle the markers.
    theme  = dsTheme ds
    theme' = theme
        { themeBulletListMarkers =
            (\ls -> T.drop 1 ls <> T.take 1 ls) <$> themeBulletListMarkers theme
        }
    ds'    = ds {dsTheme = theme'}

prettyBlock ds (OrderedList _ bss) = PP.vcat
    [ PP.indent
        (PP.Indentation 0 $ themed ds themeOrderedList $ PP.string prefix)
        (PP.Indentation 4 mempty)
        (prettyBlocks ds bs)
    | (prefix, bs) <- zip padded bss
    ] <> PP.hardline
  where
    padded  = [n ++ replicate (4 - length n) ' ' | n <- numbers]
    numbers =
        [ show i ++ "."
        | i <- [1 .. length bss]
        ]

prettyBlock _ds (RawBlock _ t) = PP.text t <> PP.hardline

prettyBlock _ds HorizontalRule = "---"

prettyBlock ds (BlockQuote bs) =
    let quote = PP.Indentation 0 (themed ds themeBlockQuote "> ") in
    PP.indent quote quote (themed ds themeBlockQuote $ prettyBlocks ds bs)

prettyBlock ds (DefinitionList terms) =
    PP.vcat $ map prettyDefinition terms
  where
    prettyDefinition (term, definitions) =
        themed ds themeDefinitionTerm (prettyInlines ds term) <$$>
        PP.hardline <> PP.vcat
        [ PP.indent
            (PP.Indentation 0 (themed ds themeDefinitionList ":   "))
            (PP.Indentation 4 mempty) $
            prettyBlocks ds (plainToPara definition)
        | definition <- definitions
        ]

    plainToPara :: [Block] -> [Block]
    plainToPara = map $ \case
        Plain inlines -> Para inlines
        block         -> block


prettyBlock ds (Table caption aligns headers rows) =
    PP.wrapAt Nothing $
    prettyTableDisplay ds TableDisplay
        { tdCaption = prettyInlines ds caption
        , tdAligns  = map align aligns
        , tdHeaders = map (prettyBlocks ds) headers
        , tdRows    = map (map (prettyBlocks ds)) rows
        }
  where
    align Pandoc.AlignLeft    = PP.AlignLeft
    align Pandoc.AlignCenter  = PP.AlignCenter
    align Pandoc.AlignDefault = PP.AlignLeft
    align Pandoc.AlignRight   = PP.AlignRight

prettyBlock ds (Div _attrs blocks) = prettyBlocks ds blocks

prettyBlock ds (LineBlock inliness) =
    let ind = PP.Indentation 0 (themed ds themeLineBlock "| ") in
    PP.wrapAt Nothing $
    PP.indent ind ind $
    PP.vcat $
    map (prettyInlines ds) inliness

prettyBlock ds (Figure _attr blocks) = prettyBlocks ds blocks

prettyBlock ds (Reveal w fragment) = prettyBlocks ds $
    revealToBlocks (dsRevealState ds) w fragment

prettyBlock ds (VarBlock var) = prettyBlocks ds $ dsResolve ds var

prettyBlock _ (SpeakerNote _) = mempty
prettyBlock _ (Config _) = mempty


--------------------------------------------------------------------------------
prettyBlocks :: DisplaySettings -> [Block] -> PP.Doc
prettyBlocks ds = PP.vcat . map (prettyBlock ds)


--------------------------------------------------------------------------------
prettyInline :: DisplaySettings -> Inline -> PP.Doc

prettyInline _ds Space = PP.space

prettyInline _ds (Str str) = PP.text str

prettyInline ds (Emph inlines) =
    themed ds themeEmph $
    prettyInlines ds inlines

prettyInline ds (Strong inlines) =
    themed ds themeStrong $
    prettyInlines ds inlines

prettyInline ds (Underline inlines) =
    themed ds themeUnderline $
    prettyInlines ds inlines

prettyInline ds (Code _ txt) =
    themed ds themeCode $
    PP.text (" " <> txt <> " ")

prettyInline ds link@(Link _attrs _text (target, _title))
    | Just (text, _, _) <- toReferenceLink link =
        let text' = prettyInlines ds text in
        "[" <>
        themed ds themeLinkText (PP.hyperlink (T.unpack target) text') <>
        "]"
    | otherwise =
        "<" <>
        themed ds themeLinkTarget
            (PP.hyperlink (T.unpack target) (PP.text target)) <>
        ">"

prettyInline _ds SoftBreak = PP.softline

prettyInline _ds LineBreak = PP.hardline

prettyInline ds (Strikeout t) =
    "~~" <> themed ds themeStrikeout (prettyInlines ds t) <> "~~"

prettyInline ds (Quoted Pandoc.SingleQuote t) =
    "'" <> themed ds themeQuoted (prettyInlines ds t) <> "'"
prettyInline ds (Quoted Pandoc.DoubleQuote t) =
    "'" <> themed ds themeQuoted (prettyInlines ds t) <> "'"

prettyInline ds (Math _ t) =
    themed ds themeMath (PP.text t)

prettyInline ds (Image _attrs text (target, _title)) =
    "![" <> themed ds themeImageText (prettyInlines ds text) <> "](" <>
    themed ds themeImageTarget (PP.text target) <> ")"

prettyInline _ (RawInline _ t) = PP.text t

-- These elements aren't really supported.
prettyInline ds  (Cite      _ t) = prettyInlines ds t
prettyInline ds  (Span      _ t) = prettyInlines ds t
prettyInline _   (Note        _) = mempty  -- TODO: support notes?
prettyInline ds  (Superscript t) = prettyInlines ds t
prettyInline ds  (Subscript   t) = prettyInlines ds t
prettyInline ds  (SmallCaps   t) = prettyInlines ds t
-- prettyInline unsupported = PP.ondullred $ PP.string $ show unsupported


--------------------------------------------------------------------------------
prettyInlines :: DisplaySettings -> [Inline] -> PP.Doc
prettyInlines ds = mconcat . map (prettyInline ds)


--------------------------------------------------------------------------------
type Reference = ([Inline], T.Text, T.Text)


--------------------------------------------------------------------------------
prettyReferences :: DisplaySettings -> [Block] -> [PP.Doc]
prettyReferences ds =
    map prettyReference . execWriter . dftBlocks (pure . pure) tellReference
  where
    tellReference :: Inline -> Writer [Reference] [Inline]
    tellReference inline = do
        for_ (toReferenceLink inline) (tell . pure)
        pure [inline]

    prettyReference :: Reference -> PP.Doc
    prettyReference (text, target, title) =
        "[" <>
        themed ds themeLinkText
            (prettyInlines ds $ newlineToSpace text) <>
        "](" <>
        themed ds themeLinkTarget (PP.text target) <>
        (if T.null title
            then mempty
            else PP.space <> "\"" <> PP.text title <> "\"")
        <> ")"

    newlineToSpace :: [Inline] -> [Inline]
    newlineToSpace = runIdentity . dftInlines (pure . pure) work
      where
        work x = pure $ case x of
            SoftBreak -> [Space]
            LineBreak -> [Space]
            _         -> [x]


--------------------------------------------------------------------------------
toReferenceLink :: Inline -> Maybe Reference
toReferenceLink (Link _attrs text (target, title))
    | [Str target] /= text = Just (text, target, title)
toReferenceLink _ = Nothing
