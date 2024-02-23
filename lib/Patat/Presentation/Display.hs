--------------------------------------------------------------------------------
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
import qualified Data.Aeson.Extended                  as A
import           Data.Char.WCWidth.Extended           (wcstrwidth)
import           Data.Data.Extended                   (grecQ)
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe, maybeToList)
import qualified Data.Sequence.Extended               as Seq
import qualified Data.Text                            as T
import qualified Patat.Presentation.Comments          as Comments
import           Patat.Presentation.Display.CodeBlock
import           Patat.Presentation.Display.Internal
import           Patat.Presentation.Display.Table
import           Patat.Presentation.Internal
import           Patat.Presentation.Settings
import           Patat.PrettyPrint                    ((<$$>), (<+>))
import qualified Patat.PrettyPrint                    as PP
import           Patat.Size
import           Patat.Theme                          (Theme (..))
import qualified Patat.Theme                          as Theme
import           Prelude
import qualified Text.Pandoc.Extended                 as Pandoc
import qualified Text.Pandoc.Writers.Shared           as Pandoc


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
    (sidx, _) = pActiveFragment
    settings  = activeSettings pres
    ds        = DisplaySettings
        { dsSize          = canvasSize
        , dsMargins       = margins settings
        , dsWrap          = fromMaybe NoWrap $ psWrap settings
        , dsTabStop       = maybe 4 A.unFlexibleNum $ psTabStop settings
        , dsTheme         = fromMaybe Theme.defaultTheme (psTheme settings)
        , dsSyntaxMap     = pSyntaxMap
        }

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
        Just (ActiveContent fragment)
                | Just _ <- psImages pSettings
                , Just image <- onlyImage fragment ->
            DisplayImage $ T.unpack image
        Just (ActiveContent fragment) -> DisplayDoc $
            displayWithBorders size pres $ \theme ->
                prettyFragment theme fragment
        Just (ActiveTitle block) -> DisplayDoc $
            displayWithBorders size pres $ \ds ->
                let auto = Margins {mTop = Auto, mRight = Auto, mLeft = Auto} in
                prettyFragment ds {dsMargins = auto} $ Fragment [block]

  where
    -- Check if the fragment consists of "just a single image".  Discard
    -- headers.
    onlyImage (Fragment (Pandoc.Header{} : bs)) = onlyImage (Fragment bs)
    onlyImage (Fragment bs) = case bs of
        [Pandoc.Figure _ _ bs']                      -> onlyImage (Fragment bs')
        [Pandoc.Para [Pandoc.Image _ _ (target, _)]] -> Just target
        _                                            -> Nothing


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
        dumpComment slide <> L.intercalate ["{fragment}"]
            [ dumpFragment (i, j)
            | j <- [0 .. numFragments slide - 1]
            ]

    dumpComment :: Slide -> [PP.Doc]
    dumpComment slide = do
        guard (Comments.cSpeakerNotes comment /= mempty)
        pure $ PP.text $ "{speakerNotes: " <>
            Comments.speakerNotesToText (Comments.cSpeakerNotes comment) <> "}"
      where
        comment = slideComment slide

    dumpFragment :: Index -> [PP.Doc]
    dumpFragment idx =
        case displayPresentation (getSize idx) pres {pActiveFragment = idx} of
            DisplayDoc doc        -> [doc]
            DisplayImage filepath -> [PP.string $ "{image: " ++ filepath ++ "}"]

    getSize :: Index -> Size
    getSize idx =
        let settings = activeSettings pres {pActiveFragment = idx}
            sRows    = fromMaybe 24 $ A.unFlexibleNum <$> psRows settings
            sCols    = fromMaybe 72 $ A.unFlexibleNum <$> psColumns settings in
        Size {..}


--------------------------------------------------------------------------------
prettyFragment :: DisplaySettings -> Fragment -> PP.Doc
prettyFragment ds (Fragment blocks) = vertical $
    PP.vcat (map (horizontal . prettyBlock ds) blocks) <>
    case prettyReferences ds blocks of
        []   -> mempty
        refs -> PP.hardline <> PP.vcat (map horizontal refs)
  where
    Size rows columns = dsSize ds
    Margins {..} = dsMargins ds

    vertical doc0 =
        mconcat (replicate top PP.hardline) <> doc0
      where
        top = case mTop of
            Auto -> let (r, _) = PP.dimensions doc0 in (rows - r) `div` 2
            NotAuto x -> x

    horizontal = horizontalIndent . horizontalWrap

    horizontalIndent doc0 = PP.indent indentation indentation doc1
      where
        doc1 = case (mLeft, mRight) of
            (Auto, Auto) -> PP.deindent doc0
            _            -> doc0
        (_, dcols) = PP.dimensions doc1
        left = case mLeft of
            NotAuto x -> x
            Auto      -> case mRight of
                NotAuto _ -> 0
                Auto      -> (columns - dcols) `div` 2
        indentation = PP.Indentation left mempty

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
prettyBlock :: DisplaySettings -> Pandoc.Block -> PP.Doc

prettyBlock ds (Pandoc.Plain inlines) = prettyInlines ds inlines

prettyBlock ds (Pandoc.Para inlines) =
    prettyInlines ds inlines <> PP.hardline

prettyBlock ds (Pandoc.Header i _ inlines) =
    themed ds themeHeader (PP.string (replicate i '#') <+> prettyInlines ds inlines) <>
    PP.hardline

prettyBlock ds (Pandoc.CodeBlock (_, classes, _) txt) =
    prettyCodeBlock ds classes txt

prettyBlock ds (Pandoc.BulletList bss) = PP.vcat
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

prettyBlock ds (Pandoc.OrderedList _ bss) = PP.vcat
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

prettyBlock _ds (Pandoc.RawBlock _ t) = PP.text t <> PP.hardline

prettyBlock _ds Pandoc.HorizontalRule = "---"

prettyBlock ds (Pandoc.BlockQuote bs) =
    let quote = PP.Indentation 0 (themed ds themeBlockQuote "> ") in
    PP.indent quote quote (themed ds themeBlockQuote $ prettyBlocks ds bs)

prettyBlock ds (Pandoc.DefinitionList terms) =
    PP.vcat $ map prettyDefinition terms
  where
    prettyDefinition (term, definitions) =
        themed ds themeDefinitionTerm (prettyInlines ds term) <$$>
        PP.hardline <> PP.vcat
        [ PP.indent
            (PP.Indentation 0 (themed ds themeDefinitionList ":   "))
            (PP.Indentation 4 mempty) $
            prettyBlocks ds (Pandoc.plainToPara definition)
        | definition <- definitions
        ]

prettyBlock ds (Pandoc.Table _ caption specs thead tbodies tfoot) =
    PP.wrapAt Nothing $
    prettyTable ds Table
        { tCaption = prettyInlines ds caption'
        , tAligns  = map align aligns
        , tHeaders = map (prettyBlocks ds) headers
        , tRows    = map (map (prettyBlocks ds)) rows
        }
  where
    (caption', aligns, _, headers, rows) = Pandoc.toLegacyTable
        caption specs thead tbodies tfoot

    align Pandoc.AlignLeft    = PP.AlignLeft
    align Pandoc.AlignCenter  = PP.AlignCenter
    align Pandoc.AlignDefault = PP.AlignLeft
    align Pandoc.AlignRight   = PP.AlignRight

prettyBlock ds (Pandoc.Div _attrs blocks) = prettyBlocks ds blocks

prettyBlock ds (Pandoc.LineBlock inliness) =
    let ind = PP.Indentation 0 (themed ds themeLineBlock "| ") in
    PP.wrapAt Nothing $
    PP.indent ind ind $
    PP.vcat $
    map (prettyInlines ds) inliness

prettyBlock ds (Pandoc.Figure _attr _caption blocks) =
    prettyBlocks ds blocks


--------------------------------------------------------------------------------
prettyBlocks :: DisplaySettings -> [Pandoc.Block] -> PP.Doc
prettyBlocks ds = PP.vcat . map (prettyBlock ds)


--------------------------------------------------------------------------------
prettyInline :: DisplaySettings -> Pandoc.Inline -> PP.Doc

prettyInline _ds Pandoc.Space = PP.space

prettyInline _ds (Pandoc.Str str) = PP.text str

prettyInline ds (Pandoc.Emph inlines) =
    themed ds themeEmph $
    prettyInlines ds inlines

prettyInline ds (Pandoc.Strong inlines) =
    themed ds themeStrong $
    prettyInlines ds inlines

prettyInline ds (Pandoc.Underline inlines) =
    themed ds themeUnderline $
    prettyInlines ds inlines

prettyInline ds (Pandoc.Code _ txt) =
    themed ds themeCode $
    PP.text (" " <> txt <> " ")

prettyInline ds link@(Pandoc.Link _attrs text (target, _title))
    | isReferenceLink link =
        "[" <> themed ds themeLinkText (prettyInlines ds text) <> "]"
    | otherwise =
        "<" <> themed ds themeLinkTarget (PP.text target) <> ">"

prettyInline _ds Pandoc.SoftBreak = PP.softline

prettyInline _ds Pandoc.LineBreak = PP.hardline

prettyInline ds (Pandoc.Strikeout t) =
    "~~" <> themed ds themeStrikeout (prettyInlines ds t) <> "~~"

prettyInline ds (Pandoc.Quoted Pandoc.SingleQuote t) =
    "'" <> themed ds themeQuoted (prettyInlines ds t) <> "'"
prettyInline ds (Pandoc.Quoted Pandoc.DoubleQuote t) =
    "'" <> themed ds themeQuoted (prettyInlines ds t) <> "'"

prettyInline ds (Pandoc.Math _ t) =
    themed ds themeMath (PP.text t)

prettyInline ds (Pandoc.Image _attrs text (target, _title)) =
    "![" <> themed ds themeImageText (prettyInlines ds text) <> "](" <>
    themed ds themeImageTarget (PP.text target) <> ")"

prettyInline _ (Pandoc.RawInline _ t) = PP.text t

-- These elements aren't really supported.
prettyInline ds  (Pandoc.Cite      _ t) = prettyInlines ds t
prettyInline ds  (Pandoc.Span      _ t) = prettyInlines ds t
prettyInline ds  (Pandoc.Note        t) = prettyBlocks  ds t
prettyInline ds  (Pandoc.Superscript t) = prettyInlines ds t
prettyInline ds  (Pandoc.Subscript   t) = prettyInlines ds t
prettyInline ds  (Pandoc.SmallCaps   t) = prettyInlines ds t
-- prettyInline unsupported = PP.ondullred $ PP.string $ show unsupported


--------------------------------------------------------------------------------
prettyInlines :: DisplaySettings -> [Pandoc.Inline] -> PP.Doc
prettyInlines ds = mconcat . map (prettyInline ds)


--------------------------------------------------------------------------------
prettyReferences :: DisplaySettings -> [Pandoc.Block] -> [PP.Doc]
prettyReferences ds =
    map prettyReference . getReferences
  where
    getReferences :: [Pandoc.Block] -> [Pandoc.Inline]
    getReferences = filter isReferenceLink . grecQ

    prettyReference :: Pandoc.Inline -> PP.Doc
    prettyReference (Pandoc.Link _attrs text (target, title)) =
        "[" <>
        themed ds themeLinkText
            (prettyInlines ds $ Pandoc.newlineToSpace text) <>
        "](" <>
        themed ds themeLinkTarget (PP.text target) <>
        (if T.null title
            then mempty
            else PP.space <> "\"" <> PP.text title <> "\"")
        <> ")"
    prettyReference _ = mempty


--------------------------------------------------------------------------------
isReferenceLink :: Pandoc.Inline -> Bool
isReferenceLink (Pandoc.Link _attrs text (target, _)) =
    [Pandoc.Str target] /= text
isReferenceLink _ = False
