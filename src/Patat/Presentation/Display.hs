--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Presentation.Display
    ( displayPresentation
    , dumpPresentation
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative              ((<$>))
import           Control.Monad                    (mplus)
import qualified Data.Aeson.Extended              as A
import           Data.Data.Extended               (grecQ)
import           Data.List                        (intersperse)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (mconcat, mempty, (<>))
import           Patat.Presentation.Display.Table
import           Patat.Presentation.Internal
import           Patat.PrettyPrint                ((<$$>), (<+>))
import qualified Patat.PrettyPrint                as PP
import           Patat.Theme                      (Theme (..))
import qualified Patat.Theme                      as Theme
import qualified System.Console.ANSI              as Ansi
import qualified System.Console.Terminal.Size     as Terminal
import qualified Text.Pandoc.Extended             as Pandoc
import           Prelude


--------------------------------------------------------------------------------
displayPresentation :: Presentation -> IO ()
displayPresentation Presentation {..} = do
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

    let theme       = fromMaybe Theme.defaultTheme (psTheme pSettings)
        title       = PP.toString (prettyInlines theme pTitle)
        titleWidth  = length title
        titleOffset = (columns - titleWidth) `div` 2
        borders     = themed (themeBorders theme)

    Ansi.setCursorColumn titleOffset
    PP.putDoc $ borders $ PP.string title
    putStrLn ""
    putStrLn ""

    let slide = case drop pActiveSlide pSlides of
            []      -> mempty
            (s : _) -> s

    PP.putDoc $ prettySlide theme slide
    putStrLn ""

    let active      = show (pActiveSlide + 1) ++ " / " ++ show (length pSlides)
        activeWidth = length active

    Ansi.setCursorPosition (rows - 2) 0
    PP.putDoc $ " " <> borders (prettyInlines theme pAuthor)
    Ansi.setCursorColumn (columns - activeWidth - 1)
    PP.putDoc $ borders $ PP.string active
    putStrLn ""


--------------------------------------------------------------------------------
dumpPresentation :: Presentation -> IO ()
dumpPresentation pres =
    let theme = fromMaybe Theme.defaultTheme (psTheme $ pSettings pres) in
    PP.putDoc $ PP.vcat $ intersperse "----------" $
        map (prettySlide theme) $ pSlides pres


--------------------------------------------------------------------------------
prettySlide :: Theme -> Slide -> PP.Doc
prettySlide theme slide@(Slide blocks) =
    prettyBlocks theme blocks <>
    case prettyReferences theme slide of
        []   -> mempty
        refs -> PP.newline <> PP.vcat refs


--------------------------------------------------------------------------------
prettyBlock :: Theme -> Pandoc.Block -> PP.Doc

prettyBlock theme (Pandoc.Plain inlines) = prettyInlines theme inlines

prettyBlock theme (Pandoc.Para inlines) =
    prettyInlines theme inlines <> PP.newline

prettyBlock theme@Theme {..} (Pandoc.Header i _ inlines) =
    themed themeHeader (PP.string (replicate i '#') <+> prettyInlines theme inlines) <>
    PP.newline

prettyBlock Theme {..} (Pandoc.CodeBlock _ txt) = PP.vcat
    [ let ind = PP.NotTrimmable "   " in
      PP.indent ind ind $ themed themeCodeBlock $ PP.string line
    | line <- blockified txt
    ] <> PP.newline
  where
    blockified str =
        let ls       = lines str
            longest  = foldr max 0 (map length ls)
            extend l = " " ++ l ++ replicate (longest - length l) ' ' ++ " " in
        map extend $ [""] ++ ls ++ [""]

prettyBlock theme (Pandoc.BulletList bss) = PP.vcat
    [ PP.indent
        (PP.NotTrimmable $ themed (themeBulletList theme) prefix)
        (PP.Trimmable "    ")
        (prettyBlocks theme' bs)
    | bs <- bss
    ] <> PP.newline
  where
    prefix = "  " <> PP.string [marker] <> " "
    marker = case themeBulletListMarkers theme of
        Just (x : _) -> x
        _            -> '-'

    -- Cycle the markers.
    theme' = theme
        { themeBulletListMarkers =
            (\ls -> drop 1 ls ++ take 1 ls) <$> themeBulletListMarkers theme
        }

prettyBlock theme@Theme {..} (Pandoc.OrderedList _ bss) = PP.vcat
    [ PP.indent
        (PP.NotTrimmable $ themed themeOrderedList $ PP.string prefix)
        (PP.Trimmable "    ")
        (prettyBlocks theme bs)
    | (prefix, bs) <- zip padded bss
    ] <> PP.newline
  where
    padded  = [n ++ replicate (4 - length n) ' ' | n <- numbers]
    numbers =
        [ show i ++ "."
        | i <- [1 .. length bss]
        ]

prettyBlock _theme (Pandoc.RawBlock _ t) = PP.string t <> PP.newline

prettyBlock _theme Pandoc.HorizontalRule = "---"

prettyBlock theme@Theme {..} (Pandoc.BlockQuote bs) =
    let quote = PP.NotTrimmable (themed themeBlockQuote "> ") in
    PP.indent quote quote (prettyBlocks theme bs)

prettyBlock theme@Theme {..} (Pandoc.DefinitionList terms) =
    PP.vcat $ map prettyDefinition terms
  where
    prettyDefinition (term, definitions) =
        themed themeDefinitionTerm (prettyInlines theme term) <$$>
        PP.newline <> PP.vcat
        [ PP.indent
            (PP.NotTrimmable (themed themeDefinitionList ":   "))
            (PP.Trimmable "    ") $
            prettyBlocks theme (Pandoc.plainToPara definition)
        | definition <- definitions
        ]

prettyBlock theme (Pandoc.Table caption aligns _ headers rows) =
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


--------------------------------------------------------------------------------
prettyBlocks :: Theme -> [Pandoc.Block] -> PP.Doc
prettyBlocks theme = PP.vcat . map (prettyBlock theme)


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
    " " <> PP.string txt <> " "

prettyInline theme@Theme {..} link@(Pandoc.Link _attrs text (target, _title))
    | isReferenceLink link =
        "[" <> themed themeLinkText (prettyInlines theme text) <> "]"
    | otherwise =
        "<" <> themed themeLinkTarget (PP.string target) <> ">"

prettyInline _theme Pandoc.SoftBreak = PP.newline

prettyInline _theme Pandoc.LineBreak = PP.newline

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
prettyReferences :: Theme -> Slide -> [PP.Doc]
prettyReferences theme@Theme {..} =
    map prettyReference . getReferences . unSlide
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
