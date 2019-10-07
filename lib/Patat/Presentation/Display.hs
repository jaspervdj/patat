--------------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Presentation.Display
    ( displayPresentation
    , displayPresentationError
    , dumpPresentation
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative                  ((<$>))
import           Control.Monad                        (mplus, unless)
import qualified Data.Aeson.Extended                  as A
import           Data.Data.Extended                   (grecQ)
import qualified Data.List                            as L
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid                          (mconcat, mempty, (<>))
import qualified Data.Text                            as T
import           Patat.Cleanup
import qualified Patat.Images                         as Images
import           Patat.Presentation.Display.CodeBlock
import           Patat.Presentation.Display.Table
import           Patat.Presentation.Internal
import           Patat.PrettyPrint                    ((<$$>), (<+>))
import qualified Patat.PrettyPrint                    as PP
import           Patat.Theme                          (Theme (..))
import qualified Patat.Theme                          as Theme
import           Prelude
import qualified System.Console.ANSI                  as Ansi
import qualified System.Console.Terminal.Size         as Terminal
import qualified System.IO                            as IO
import qualified Text.Pandoc.Extended                 as Pandoc


--------------------------------------------------------------------------------
data CanvasSize = CanvasSize {csRows :: Int, csCols :: Int} deriving (Show)


--------------------------------------------------------------------------------
-- | Display something within the presentation borders that draw the title and
-- the active slide number and so on.
displayWithBorders
    :: Presentation -> (CanvasSize -> Theme -> PP.Doc) -> IO Cleanup
displayWithBorders Presentation {..} f = do
    Ansi.clearScreen
    Ansi.setCursorPosition 0 0

    -- Get terminal width/title
    mbWindow <- Terminal.size
    let columns = fromMaybe 72 $
            (A.unFlexibleNum <$> psColumns pSettings) `mplus`
            (Terminal.width  <$> mbWindow)
        rows    = fromMaybe 24 $
            (A.unFlexibleNum <$> psRows pSettings) `mplus`
            (Terminal.height <$> mbWindow)

    let settings    = pSettings {psColumns = Just $ A.FlexibleNum columns}
        theme       = fromMaybe Theme.defaultTheme (psTheme settings)
        title       = PP.toString (prettyInlines theme pTitle)
        titleWidth  = length title
        titleOffset = (columns - titleWidth) `div` 2
        borders     = themed (themeBorders theme)

    unless (null title) $ do
        let titleRemainder = columns - titleWidth - titleOffset
            wrappedTitle = PP.spaces titleOffset <> PP.string title <> PP.spaces titleRemainder
        PP.putDoc $ borders wrappedTitle
        putStrLn ""
        putStrLn ""

    let canvasSize = CanvasSize (rows - 2) columns
    PP.putDoc $ formatWith settings $ f canvasSize theme
    putStrLn ""

    let (sidx, _)    = pActiveFragment
        active       = show (sidx + 1) ++ " / " ++ show (length pSlides)
        activeWidth  = length active
        author       = PP.toString (prettyInlines theme pAuthor)
        authorWidth  = length author
        middleSpaces = PP.spaces $ columns - activeWidth - authorWidth - 2

    Ansi.setCursorPosition (rows - 1) 0
    PP.putDoc $ borders $ PP.space <> PP.string author <> middleSpaces <> PP.string active <> PP.space
    IO.hFlush IO.stdout

    return mempty


--------------------------------------------------------------------------------
displayImage :: Images.Handle -> FilePath -> IO Cleanup
displayImage images path = do
    Ansi.clearScreen
    Ansi.setCursorPosition 0 0
    putStrLn ""
    IO.hFlush IO.stdout
    Images.drawImage images path


--------------------------------------------------------------------------------
displayPresentation :: Maybe Images.Handle -> Presentation -> IO Cleanup
displayPresentation mbImages pres@Presentation {..} =
     case getActiveFragment pres of
        Nothing                       -> displayWithBorders pres mempty
        Just (ActiveContent fragment)
                | Just images <- mbImages
                , Just image <- onlyImage fragment ->
            displayImage images image
        Just (ActiveContent fragment) ->
            displayWithBorders pres $ \_canvasSize theme ->
            prettyFragment theme fragment
        Just (ActiveTitle   block)    ->
            displayWithBorders pres $ \canvasSize theme ->
            let pblock          = prettyBlock theme block
                (prows, pcols)  = PP.dimensions pblock
                (mLeft, mRight) = marginsOf pSettings
                offsetRow       = (csRows canvasSize `div` 2) - (prows `div` 2)
                offsetCol       = ((csCols canvasSize - mLeft - mRight) `div` 2) - (pcols `div` 2)
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
displayPresentationError :: Presentation -> String -> IO Cleanup
displayPresentationError pres err = displayWithBorders pres $ \_ Theme {..} ->
    themed themeStrong "Error occurred in the presentation:" <$$>
    "" <$$>
    (PP.string err)


--------------------------------------------------------------------------------
dumpPresentation :: Presentation -> IO ()
dumpPresentation pres =
    let settings = pSettings pres
        theme    = fromMaybe Theme.defaultTheme (psTheme $ settings) in
    PP.putDoc $ formatWith settings $
        PP.vcat $ L.intersperse "----------" $ do
            slide <- pSlides pres
            return $ case slide of
                TitleSlide   block     -> "~~~title" <$$> prettyBlock theme block
                ContentSlide fragments -> PP.vcat $ L.intersperse "~~~frag" $ do
                    fragment <- fragments
                    return $ prettyFragment theme fragment


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
prettyFragment theme fragment@(Fragment blocks) =
    prettyBlocks theme blocks <>
    case prettyReferences theme fragment of
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

prettyBlock _theme (Pandoc.RawBlock _ t) = PP.string t <> PP.hardline

prettyBlock _theme Pandoc.HorizontalRule = "---"

prettyBlock theme@Theme {..} (Pandoc.BlockQuote bs) =
    let quote = PP.NotTrimmable (themed themeBlockQuote "> ") in
    PP.indent quote quote (prettyBlocks theme bs)

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

prettyBlock theme (Pandoc.Table caption aligns _ headers rows) =
    PP.wrapAt Nothing $
    prettyTable theme Table
        { tCaption = prettyInlines theme caption
        , tAligns  = map align aligns
        , tHeaders = map (prettyBlocks theme) headers
        , tRows    = map (map (prettyBlocks theme)) rows
        }
  where
    align Pandoc.AlignLeft    = PP.AlignLeft
    align Pandoc.AlignCenter  = PP.AlignCenter
    align Pandoc.AlignDefault = PP.AlignLeft
    align Pandoc.AlignRight   = PP.AlignRight

prettyBlock theme (Pandoc.Div _attrs blocks) = prettyBlocks theme blocks

prettyBlock _theme Pandoc.Null = mempty

#if MIN_VERSION_pandoc(1,18,0)
-- 'LineBlock' elements are new in pandoc-1.18
prettyBlock theme@Theme {..} (Pandoc.LineBlock inliness) =
    let ind = PP.NotTrimmable (themed themeLineBlock "| ") in
    PP.wrapAt Nothing $
    PP.indent ind ind $
    PP.vcat $
    map (prettyInlines theme) inliness
#endif


--------------------------------------------------------------------------------
prettyBlocks :: Theme -> [Pandoc.Block] -> PP.Doc
prettyBlocks theme = PP.vcat . map (prettyBlock theme) . filter isVisibleBlock


--------------------------------------------------------------------------------
prettyInline :: Theme -> Pandoc.Inline -> PP.Doc

prettyInline _theme Pandoc.Space = PP.space

prettyInline _theme (Pandoc.Str str) = PP.string str

prettyInline theme@Theme {..} (Pandoc.Emph inlines) =
    themed themeEmph $
    prettyInlines theme inlines

prettyInline theme@Theme {..} (Pandoc.Strong inlines) =
    themed themeStrong $
    prettyInlines theme inlines

prettyInline Theme {..} (Pandoc.Code _ txt) =
    themed themeCode $
    PP.string (" " <> txt <> " ")

prettyInline theme@Theme {..} link@(Pandoc.Link _attrs text (target, _title))
    | isReferenceLink link =
        "[" <> themed themeLinkText (prettyInlines theme text) <> "]"
    | otherwise =
        "<" <> themed themeLinkTarget (PP.string target) <> ">"

prettyInline _theme Pandoc.SoftBreak = PP.softline

prettyInline _theme Pandoc.LineBreak = PP.hardline

prettyInline theme@Theme {..} (Pandoc.Strikeout t) =
    "~~" <> themed themeStrikeout (prettyInlines theme t) <> "~~"

prettyInline theme@Theme {..} (Pandoc.Quoted Pandoc.SingleQuote t) =
    "'" <> themed themeQuoted (prettyInlines theme t) <> "'"
prettyInline theme@Theme {..} (Pandoc.Quoted Pandoc.DoubleQuote t) =
    "'" <> themed themeQuoted (prettyInlines theme t) <> "'"

prettyInline Theme {..} (Pandoc.Math _ t) =
    themed themeMath (PP.string t)

prettyInline theme@Theme {..} (Pandoc.Image _attrs text (target, _title)) =
    "![" <> themed themeImageText (prettyInlines theme text) <> "](" <>
    themed themeImageTarget (PP.string target) <> ")"

-- These elements aren't really supported.
prettyInline theme  (Pandoc.Cite      _ t) = prettyInlines theme t
prettyInline theme  (Pandoc.Span      _ t) = prettyInlines theme t
prettyInline _theme (Pandoc.RawInline _ t) = PP.string t
prettyInline theme  (Pandoc.Note        t) = prettyBlocks  theme t
prettyInline theme  (Pandoc.Superscript t) = prettyInlines theme t
prettyInline theme  (Pandoc.Subscript   t) = prettyInlines theme t
prettyInline theme  (Pandoc.SmallCaps   t) = prettyInlines theme t
-- prettyInline unsupported = PP.ondullred $ PP.string $ show unsupported


--------------------------------------------------------------------------------
prettyInlines :: Theme -> [Pandoc.Inline] -> PP.Doc
prettyInlines theme = mconcat . map (prettyInline theme)


--------------------------------------------------------------------------------
prettyReferences :: Theme -> Fragment -> [PP.Doc]
prettyReferences theme@Theme {..} =
    map prettyReference . getReferences . unFragment
  where
    getReferences :: [Pandoc.Block] -> [Pandoc.Inline]
    getReferences = filter isReferenceLink . grecQ

    prettyReference :: Pandoc.Inline -> PP.Doc
    prettyReference (Pandoc.Link _attrs text (target, title)) =
        "[" <>
        themed themeLinkText (prettyInlines theme $ Pandoc.newlineToSpace text) <>
        "](" <>
        themed themeLinkTarget (PP.string target) <>
        (if null title
            then mempty
            else PP.space <> "\"" <> PP.string title <> "\"")
        <> ")"
    prettyReference _ = mempty


--------------------------------------------------------------------------------
isReferenceLink :: Pandoc.Inline -> Bool
isReferenceLink (Pandoc.Link _attrs text (target, _)) =
    [Pandoc.Str target] /= text
isReferenceLink _ = False


--------------------------------------------------------------------------------
isVisibleBlock :: Pandoc.Block -> Bool
isVisibleBlock Pandoc.Null = False
isVisibleBlock (Pandoc.RawBlock (Pandoc.Format "html") t) =
    not ("<!--" `L.isPrefixOf` t && "-->" `L.isSuffixOf` t)
isVisibleBlock _ = True
