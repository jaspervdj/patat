--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Presentation.Display
    ( Size
    , getDisplaySize

    , Display (..)
    , displayPresentation
    , displayPresentationError
    , dumpPresentation
    ) where


--------------------------------------------------------------------------------
import           Control.Monad                        (mplus)
import qualified Data.Aeson.Extended                  as A
import           Data.Char.WCWidth.Extended           (wcstrwidth)
import           Data.Data.Extended                   (grecQ)
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe, listToMaybe,
                                                       maybeToList)
import qualified Data.Text                            as T
import           Patat.Presentation.Display.CodeBlock
import           Patat.Presentation.Display.Table
import           Patat.Presentation.Internal
import           Patat.PrettyPrint                    ((<$$>), (<+>))
import qualified Patat.PrettyPrint                    as PP
import           Patat.Theme                          (Theme (..))
import qualified Patat.Theme                          as Theme
import           Prelude
import qualified System.Console.Terminal.Size         as Terminal
import qualified Text.Pandoc.Extended                 as Pandoc
import qualified Text.Pandoc.Writers.Shared           as Pandoc


--------------------------------------------------------------------------------
data Size = Size {sRows :: Int, sCols :: Int} deriving (Show)


--------------------------------------------------------------------------------
getDisplaySize :: Presentation -> IO Size
getDisplaySize Presentation {..} = do
    mbWindow <- Terminal.size
    let sRows = fromMaybe 24 $
            (A.unFlexibleNum <$> psRows pSettings) `mplus`
            (Terminal.height <$> mbWindow)
        sCols = fromMaybe 72 $
            (A.unFlexibleNum <$> psColumns pSettings) `mplus`
            (Terminal.width  <$> mbWindow)
    pure $ Size {..}


--------------------------------------------------------------------------------
data Display = DisplayDoc PP.Doc | DisplayImage FilePath deriving (Show)


--------------------------------------------------------------------------------
-- | Display something within the presentation borders that draw the title and
-- the active slide number and so on.
displayWithBorders
    :: Size -> Presentation -> (Size -> Theme -> PP.Doc) -> PP.Doc
displayWithBorders (Size rows columns) Presentation {..} f =
    (if null title
        then mempty
        else
            let titleRemainder = columns - titleWidth - titleOffset
                wrappedTitle = PP.spaces titleOffset <> PP.string title <> PP.spaces titleRemainder in
        borders wrappedTitle <> PP.hardline <> PP.hardline) <>
    formatWith settings (f canvasSize theme) <> PP.hardline <>
    PP.goToLine (rows - 2) <>
    borders (PP.space <> PP.string author <> middleSpaces <> PP.string active <> PP.space) <>
    PP.hardline
  where
    -- Get terminal width/title
    (sidx, _)   = pActiveFragment
    settings    = pSettings {psColumns = Just $ A.FlexibleNum columns}
    theme       = fromMaybe Theme.defaultTheme (psTheme settings)

    -- Compute title.
    breadcrumbs = fromMaybe [] . listToMaybe $ drop sidx pBreadcrumbs
    plainTitle  = PP.toString $ prettyInlines theme pTitle
    breadTitle  = mappend plainTitle $ mconcat
        [ s
        | b <- map (prettyInlines theme . snd) breadcrumbs
        , s <- [" > ", PP.toString b]
        ]
    title
        | not . fromMaybe True $ psBreadcrumbs settings = plainTitle
        | wcstrwidth breadTitle > columns               = plainTitle
        | otherwise                                     = breadTitle

    -- Dimensions of title.
    titleWidth  = wcstrwidth title
    titleOffset = (columns - titleWidth) `div` 2
    borders     = themed (themeBorders theme)

    -- Room left for content
    canvasSize = Size (rows - 2) columns

    -- Compute footer.
    active
        | fromMaybe True $ psSlideNumber settings = show (sidx + 1) ++ " / " ++ show (length pSlides)
        | otherwise                               = ""
    activeWidth  = wcstrwidth active
    author       = PP.toString (prettyInlines theme pAuthor)
    authorWidth  = wcstrwidth author
    middleSpaces = PP.spaces $ columns - activeWidth - authorWidth - 2


--------------------------------------------------------------------------------
displayPresentation :: Size -> Presentation -> Display
displayPresentation size pres@Presentation {..} =
     case getActiveFragment pres of
        Nothing -> DisplayDoc $ displayWithBorders size pres mempty
        Just (ActiveContent fragment)
                | Just _ <- psImages pSettings
                , Just image <- onlyImage fragment ->
            DisplayImage $ T.unpack image
        Just (ActiveContent fragment) -> DisplayDoc $
            displayWithBorders size pres $ \_canvasSize theme ->
                prettyFragment theme fragment
        Just (ActiveTitle block) -> DisplayDoc $
            displayWithBorders size pres $ \canvasSize theme ->
            let pblock          = prettyBlock theme block
                (prows, pcols)  = PP.dimensions pblock
                (mLeft, mRight) = marginsOf pSettings
                offsetRow       = (sRows canvasSize `div` 2) - (prows `div` 2)
                offsetCol       = ((sCols canvasSize - mLeft - mRight) `div` 2) - (pcols `div` 2)
                spaces          = PP.NotTrimmable $ PP.spaces offsetCol in
            mconcat (replicate (offsetRow - 3) PP.hardline) <$$>
            PP.indent spaces spaces pblock

  where
    -- Check if the fragment consists of just a single image, or a header and
    -- some image.
    onlyImage (Fragment blocks)
            | [Pandoc.Para para] <- filter isVisibleBlock blocks
            , [Pandoc.Image _ _ (target, _)] <- para =
        Just target
    onlyImage (Fragment blocks)
            | [Pandoc.Header _ _ _, Pandoc.Para para] <- filter isVisibleBlock blocks
            , [Pandoc.Image _ _ (target, _)] <- para =
        Just target
    onlyImage _ = Nothing


--------------------------------------------------------------------------------
-- | Displays an error in the place of the presentation.  This is useful if we
-- want to display an error but keep the presentation running.
displayPresentationError :: Size -> Presentation -> String -> PP.Doc
displayPresentationError size pres err =
    displayWithBorders size pres $ \_ Theme {..} ->
        themed themeStrong "Error occurred in the presentation:" <$$>
        "" <$$>
        (PP.string err)


--------------------------------------------------------------------------------
dumpPresentation :: Presentation -> IO ()
dumpPresentation pres@Presentation {..} =
    let sRows = fromMaybe 24 $ A.unFlexibleNum <$> psRows pSettings
        sCols = fromMaybe 72 $ A.unFlexibleNum <$> psColumns pSettings
        size  = Size {..} in
    PP.putDoc $ PP.removeControls $ formatWith pSettings $
    PP.vcat $ L.intersperse "----------" $ do
        i <- [0 .. length pSlides - 1]
        slide <- maybeToList $ getSlide i pres
        j <- [0 .. numFragments slide - 1]
        case displayPresentation size pres {pActiveFragment = (i, j)} of
            DisplayDoc doc -> [doc]
            DisplayImage _ -> []


--------------------------------------------------------------------------------
formatWith :: PresentationSettings -> PP.Doc -> PP.Doc
formatWith ps = wrap . indent
  where
    (marginLeft, marginRight) = marginsOf ps
    wrap = case (psWrap ps, psColumns ps) of
        (Just True,  Just (A.FlexibleNum col)) -> PP.wrapAt (Just $ col - marginRight)
        _                                      -> id
    spaces = PP.NotTrimmable $ PP.spaces marginLeft
    indent = PP.indent spaces spaces


--------------------------------------------------------------------------------
prettyFragment :: Theme -> Fragment -> PP.Doc
prettyFragment theme (Fragment blocks) =
    prettyBlocks theme blocks <>
    case prettyReferences theme blocks of
        []   -> mempty
        refs -> PP.hardline <> PP.vcat refs


--------------------------------------------------------------------------------
prettyBlock :: Theme -> Pandoc.Block -> PP.Doc

prettyBlock theme (Pandoc.Plain inlines) = prettyInlines theme inlines

prettyBlock theme (Pandoc.Para inlines) =
    prettyInlines theme inlines <> PP.hardline

prettyBlock theme@Theme {..} (Pandoc.Header i _ inlines) =
    themed themeHeader (PP.string (replicate i '#') <+> prettyInlines theme inlines) <>
    PP.hardline

prettyBlock theme (Pandoc.CodeBlock (_, classes, _) txt) =
    prettyCodeBlock theme classes txt

prettyBlock theme (Pandoc.BulletList bss) = PP.vcat
    [ PP.indent
        (PP.NotTrimmable $ themed (themeBulletList theme) prefix)
        (PP.Trimmable "    ")
        (prettyBlocks theme' bs)
    | bs <- bss
    ] <> PP.hardline
  where
    prefix = "  " <> PP.string [marker] <> " "
    marker = case T.unpack <$> themeBulletListMarkers theme of
        Just (x : _) -> x
        _            -> '-'

    -- Cycle the markers.
    theme' = theme
        { themeBulletListMarkers =
            (\ls -> T.drop 1 ls <> T.take 1 ls) <$> themeBulletListMarkers theme
        }

prettyBlock theme@Theme {..} (Pandoc.OrderedList _ bss) = PP.vcat
    [ PP.indent
        (PP.NotTrimmable $ themed themeOrderedList $ PP.string prefix)
        (PP.Trimmable "    ")
        (prettyBlocks theme bs)
    | (prefix, bs) <- zip padded bss
    ] <> PP.hardline
  where
    padded  = [n ++ replicate (4 - length n) ' ' | n <- numbers]
    numbers =
        [ show i ++ "."
        | i <- [1 .. length bss]
        ]

prettyBlock _theme (Pandoc.RawBlock _ t) = PP.text t <> PP.hardline

prettyBlock _theme Pandoc.HorizontalRule = "---"

prettyBlock theme@Theme {..} (Pandoc.BlockQuote bs) =
    let quote = PP.NotTrimmable (themed themeBlockQuote "> ") in
    PP.indent quote quote (themed themeBlockQuote $ prettyBlocks theme bs)

prettyBlock theme@Theme {..} (Pandoc.DefinitionList terms) =
    PP.vcat $ map prettyDefinition terms
  where
    prettyDefinition (term, definitions) =
        themed themeDefinitionTerm (prettyInlines theme term) <$$>
        PP.hardline <> PP.vcat
        [ PP.indent
            (PP.NotTrimmable (themed themeDefinitionList ":   "))
            (PP.Trimmable "    ") $
            prettyBlocks theme (Pandoc.plainToPara definition)
        | definition <- definitions
        ]

prettyBlock theme (Pandoc.Table _ caption specs thead tbodies tfoot) =
    PP.wrapAt Nothing $
    prettyTable theme Table
        { tCaption = prettyInlines theme caption'
        , tAligns  = map align aligns
        , tHeaders = map (prettyBlocks theme) headers
        , tRows    = map (map (prettyBlocks theme)) rows
        }
  where
    (caption', aligns, _, headers, rows) = Pandoc.toLegacyTable
        caption specs thead tbodies tfoot

    align Pandoc.AlignLeft    = PP.AlignLeft
    align Pandoc.AlignCenter  = PP.AlignCenter
    align Pandoc.AlignDefault = PP.AlignLeft
    align Pandoc.AlignRight   = PP.AlignRight

prettyBlock theme (Pandoc.Div _attrs blocks) = prettyBlocks theme blocks

prettyBlock theme@Theme {..} (Pandoc.LineBlock inliness) =
    let ind = PP.NotTrimmable (themed themeLineBlock "| ") in
    PP.wrapAt Nothing $
    PP.indent ind ind $
    PP.vcat $
    map (prettyInlines theme) inliness

prettyBlock theme (Pandoc.Figure _attr _caption blocks) =
    prettyBlocks theme blocks

--------------------------------------------------------------------------------
prettyBlocks :: Theme -> [Pandoc.Block] -> PP.Doc
prettyBlocks theme = PP.vcat . map (prettyBlock theme) . filter isVisibleBlock


--------------------------------------------------------------------------------
prettyInline :: Theme -> Pandoc.Inline -> PP.Doc

prettyInline _theme Pandoc.Space = PP.space

prettyInline _theme (Pandoc.Str str) = PP.text str

prettyInline theme@Theme {..} (Pandoc.Emph inlines) =
    themed themeEmph $
    prettyInlines theme inlines

prettyInline theme@Theme {..} (Pandoc.Strong inlines) =
    themed themeStrong $
    prettyInlines theme inlines

prettyInline theme@Theme {..} (Pandoc.Underline inlines) =
    themed themeUnderline $
    prettyInlines theme inlines

prettyInline Theme {..} (Pandoc.Code _ txt) =
    themed themeCode $
    PP.text (" " <> txt <> " ")

prettyInline theme@Theme {..} link@(Pandoc.Link _attrs text (target, _title))
    | isReferenceLink link =
        "[" <> themed themeLinkText (prettyInlines theme text) <> "]"
    | otherwise =
        "<" <> themed themeLinkTarget (PP.text target) <> ">"

prettyInline _theme Pandoc.SoftBreak = PP.softline

prettyInline _theme Pandoc.LineBreak = PP.hardline

prettyInline theme@Theme {..} (Pandoc.Strikeout t) =
    "~~" <> themed themeStrikeout (prettyInlines theme t) <> "~~"

prettyInline theme@Theme {..} (Pandoc.Quoted Pandoc.SingleQuote t) =
    "'" <> themed themeQuoted (prettyInlines theme t) <> "'"
prettyInline theme@Theme {..} (Pandoc.Quoted Pandoc.DoubleQuote t) =
    "'" <> themed themeQuoted (prettyInlines theme t) <> "'"

prettyInline Theme {..} (Pandoc.Math _ t) =
    themed themeMath (PP.text t)

prettyInline theme@Theme {..} (Pandoc.Image _attrs text (target, _title)) =
    "![" <> themed themeImageText (prettyInlines theme text) <> "](" <>
    themed themeImageTarget (PP.text target) <> ")"

-- These elements aren't really supported.
prettyInline theme  (Pandoc.Cite      _ t) = prettyInlines theme t
prettyInline theme  (Pandoc.Span      _ t) = prettyInlines theme t
prettyInline _theme (Pandoc.RawInline _ t) = PP.text t
prettyInline theme  (Pandoc.Note        t) = prettyBlocks  theme t
prettyInline theme  (Pandoc.Superscript t) = prettyInlines theme t
prettyInline theme  (Pandoc.Subscript   t) = prettyInlines theme t
prettyInline theme  (Pandoc.SmallCaps   t) = prettyInlines theme t
-- prettyInline unsupported = PP.ondullred $ PP.string $ show unsupported


--------------------------------------------------------------------------------
prettyInlines :: Theme -> [Pandoc.Inline] -> PP.Doc
prettyInlines theme = mconcat . map (prettyInline theme)


--------------------------------------------------------------------------------
prettyReferences :: Theme -> [Pandoc.Block] -> [PP.Doc]
prettyReferences theme@Theme {..} =
    map prettyReference . getReferences
  where
    getReferences :: [Pandoc.Block] -> [Pandoc.Inline]
    getReferences = filter isReferenceLink . grecQ

    prettyReference :: Pandoc.Inline -> PP.Doc
    prettyReference (Pandoc.Link _attrs text (target, title)) =
        "[" <>
        themed themeLinkText (prettyInlines theme $ Pandoc.newlineToSpace text) <>
        "](" <>
        themed themeLinkTarget (PP.text target) <>
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


--------------------------------------------------------------------------------
isVisibleBlock :: Pandoc.Block -> Bool
isVisibleBlock (Pandoc.RawBlock (Pandoc.Format "html") t) =
    not ("<!--" `T.isPrefixOf` t && "-->" `T.isSuffixOf` t)
isVisibleBlock _ = True
