--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Presentation.Display
    ( displayPresentation
    ) where


--------------------------------------------------------------------------------
import           Data.Monoid                  ((<>))
import           Patat.Presentation.Internal
import qualified System.Console.ANSI          as Ansi
import qualified System.Console.Terminal.Size as Terminal
import qualified Text.Pandoc                  as Pandoc
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP


--------------------------------------------------------------------------------
displayPresentation :: Presentation -> IO ()
displayPresentation Presentation {..} = do
    Ansi.clearScreen
    Ansi.setCursorPosition 0 0

    -- Get terminal width/title
    mbWindow <- Terminal.size
    let termWidth   = maybe 72 Terminal.width  mbWindow
        termHeight  = maybe 24 Terminal.height mbWindow
        title       = show (prettyInlines pTitle)
        titleWidth  = length title
        titleOffset = (termWidth - titleWidth) `div` 2

    Ansi.setCursorColumn titleOffset
    PP.putDoc $ PP.dullyellow $ PP.string title
    putStrLn ""
    putStrLn ""

    let slide = case drop pActiveSlide pSlides of
            []      -> mempty
            (s : _) -> s

    PP.putDoc $ prettySlide slide
    putStrLn ""

    let active      = show (pActiveSlide + 1) ++ " / " ++ show (length pSlides)
        activeWidth = length active

    Ansi.setCursorPosition (termHeight - 2) 0
    PP.putDoc $ " " <> PP.dullyellow (prettyInlines pAuthor)
    Ansi.setCursorColumn (termWidth - activeWidth - 1)
    PP.putDoc $ PP.dullyellow $ PP.string active
    putStrLn ""


--------------------------------------------------------------------------------
prettySlide :: Slide -> PP.Doc
prettySlide = prettyBlocks . unSlide


--------------------------------------------------------------------------------
prettyBlock :: Pandoc.Block -> PP.Doc

prettyBlock (Pandoc.Plain inlines) = prettyInlines inlines

prettyBlock (Pandoc.Para inlines) = prettyInlines inlines <> PP.line

prettyBlock (Pandoc.Header i _ inlines) =
    PP.dullblue (PP.string (replicate i '#') <+> prettyInlines inlines) <>
    PP.line

prettyBlock (Pandoc.CodeBlock _ txt) = PP.vcat
    [ PP.indent 3 $ PP.ondullblack $ PP.dullwhite $ PP.string line
    | line <- blockified txt
    ] <> PP.line
  where
    blockified str =
        let ls       = lines str
            longest  = foldr max 0 (map length ls)
            extend l = " " ++ l ++ replicate (longest - length l) ' ' ++ " " in
        map extend $ [""] ++ ls ++ [""]

prettyBlock (Pandoc.BulletList bss) = PP.vcat
    [ PP.dullmagenta "-" <+> PP.align (prettyBlocks bs)
    | bs <- bss
    ] <> PP.line

prettyBlock (Pandoc.OrderedList _ bss) = PP.vcat
    [ PP.dullmagenta (PP.string prefix) <+> PP.align (prettyBlocks bs)
    | (prefix, bs) <- zip padded bss
    ] <> PP.line
  where
    longest = foldr max 0 (map length numbers)
    padded  = [n ++ replicate (longest - length n) ' ' | n <- numbers]
    numbers =
        [ show i ++ "."
        | i <- [1 .. length bss]
        ]

prettyBlock unsupported = PP.ondullred $ PP.string $ show unsupported


--------------------------------------------------------------------------------
prettyBlocks :: [Pandoc.Block] -> PP.Doc
prettyBlocks = PP.vcat . map prettyBlock


--------------------------------------------------------------------------------
prettyInline :: Pandoc.Inline -> PP.Doc

prettyInline Pandoc.Space = PP.space

prettyInline (Pandoc.Str str) = PP.string str

prettyInline (Pandoc.Emph inlines) = PP.dullgreen $ prettyInlines inlines

prettyInline (Pandoc.Strong inlines) =
    PP.dullred $ PP.bold $ prettyInlines inlines

prettyInline (Pandoc.Code _ txt) =
    PP.ondullblack $ PP.dullwhite $ " " <> PP.string txt <> " "

prettyInline (Pandoc.Link _ title (target, _))
    | [Pandoc.Str target] == title =
        PP.dullcyan $ PP.underline $ "<" <> PP.string target <> ">"

prettyInline Pandoc.SoftBreak = PP.softline

prettyInline Pandoc.LineBreak = PP.hardline

prettyInline unsupported = PP.ondullred $ PP.string $ show unsupported


--------------------------------------------------------------------------------
prettyInlines :: [Pandoc.Inline] -> PP.Doc
prettyInlines = mconcat . map prettyInline
