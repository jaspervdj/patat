--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where


--------------------------------------------------------------------------------
import           Data.List                    (intersperse)
import           Data.Monoid                  ((<>))
import qualified System.Console.ANSI          as Ansi
import qualified System.Console.Terminal.Size as Terminal
import           System.Environment           (getArgs)
import qualified System.IO                    as IO
import qualified Text.Pandoc                  as Pandoc
import           Text.PrettyPrint.ANSI.Leijen ((<+>))
import qualified Text.PrettyPrint.ANSI.Leijen as PP


--------------------------------------------------------------------------------
data Presentation = Presentation
    { pFilePath    :: !FilePath
    , pTitle       :: ![Pandoc.Inline]
    , pAuthor      :: ![Pandoc.Inline]
    , pSlides      :: [Slide]
    , pActiveSlide :: !Int
    } deriving (Show)


--------------------------------------------------------------------------------
pandocToPresentation :: FilePath -> Pandoc.Pandoc -> Either String Presentation
pandocToPresentation pFilePath pandoc@(Pandoc.Pandoc meta _) = do
    let pTitle       = Pandoc.docTitle meta
        pSlides      = pandocToSlides pandoc
        pActiveSlide = 0
        pAuthor      = concat (Pandoc.docAuthors meta)
    return Presentation {..}


--------------------------------------------------------------------------------
displayPresentation :: Presentation -> IO ()
displayPresentation Presentation {..} = do
    Ansi.clearScreen
    Ansi.setCursorPosition 0 0

    -- Get terminal width/title
    mbWindow <- Terminal.size
    let termWidth   = maybe 80 Terminal.width  mbWindow
        termHeight  = maybe 10 Terminal.height mbWindow
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

    PP.putDoc $ PP.pretty slide
    putStrLn ""

    let active      = show (pActiveSlide + 1) ++ " / " ++ show (length pSlides)
        activeWidth = length active

    Ansi.setCursorPosition (termHeight - 2) 0
    PP.putDoc $ " " <> PP.dullyellow (prettyInlines pAuthor)
    Ansi.setCursorColumn (termWidth - activeWidth - 1)
    PP.putDoc $ PP.dullyellow $ PP.string active
    putStrLn ""


--------------------------------------------------------------------------------
updatePresentation :: String -> Presentation -> IO (Maybe Presentation)

updatePresentation char presentation = case char of
    "q"      -> return Nothing
    "\n"     -> return $ goToSlide fwd
    "\DEL"   -> return $ goToSlide bwd
    "h"      -> return $ goToSlide bwd
    "j"      -> return $ goToSlide skipFwd
    "k"      -> return $ goToSlide skipBwd
    "l"      -> return $ goToSlide fwd
    "\ESC[C" -> return $ goToSlide fwd      -- Right arrow
    "\ESC[D" -> return $ goToSlide bwd      -- Left arrow
    "\ESC[B" -> return $ goToSlide skipFwd  -- Down arrow
    "\ESC[A" -> return $ goToSlide skipBwd  -- Up arrow
    "r"      -> reloadPresentation
    _        -> return $ Just presentation
  where
    numSlides = length (pSlides presentation)
    clip idx  = min (max 0 idx) (numSlides - 1)

    fwd       = \x -> x + 1
    bwd       = \x -> x - 1
    skipFwd   = \x -> x + 10
    skipBwd   = \x -> x - 10

    goToSlide f =
        Just presentation {pActiveSlide = clip (f $ pActiveSlide presentation)}

    reloadPresentation = do
        pres <- readPresentation (pFilePath presentation)
        return $ Just pres {pActiveSlide = clip (pActiveSlide presentation)}


--------------------------------------------------------------------------------
readPresentation :: FilePath -> IO Presentation
readPresentation filePath = do
    src  <- readFile filePath
    doc  <- either (fail . show) return $ Pandoc.readMarkdown Pandoc.def src
    either fail return $ pandocToPresentation filePath doc


--------------------------------------------------------------------------------
newtype Slide = Slide {unSlide :: [Pandoc.Block]}
    deriving (Monoid, Show)


--------------------------------------------------------------------------------
instance PP.Pretty Slide where
    pretty = prettyBlocks . unSlide


--------------------------------------------------------------------------------
pandocToSlides :: Pandoc.Pandoc -> [Slide]
pandocToSlides (Pandoc.Pandoc _meta blocks0) = splitSlides blocks0
  where
    splitSlides blocks = case break (== Pandoc.HorizontalRule) blocks of
        (xs, [])           -> [Slide xs]
        (xs, (_rule : ys)) -> Slide xs : splitSlides ys


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

prettyInline unsupported = PP.ondullred $ PP.string $ show unsupported


--------------------------------------------------------------------------------
prettyInlines :: [Pandoc.Inline] -> PP.Doc
prettyInlines = mconcat . map prettyInline


--------------------------------------------------------------------------------
main :: IO ()
main = do
    (file : _) <- getArgs
    pres       <- readPresentation file

    IO.hSetBuffering IO.stdin IO.NoBuffering
    loop pres

  where
    loop pres0 = do
        displayPresentation pres0
        c       <- readKey
        mbPres1 <- updatePresentation c pres0
        case mbPres1 of
            Nothing    -> return ()
            Just pres1 -> loop pres1


--------------------------------------------------------------------------------
readKey :: IO String
readKey = do
    c0 <- getChar
    case c0 of
        '\ESC' -> do
            c1 <- getChar
            case c1 of
                '[' -> do
                    c2 <- getChar
                    return [c0, c1, c2]
                _ -> return [c0, c1]
        _ -> return [c0]
