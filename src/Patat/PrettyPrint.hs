--------------------------------------------------------------------------------
-- | This is a small pretty-printing library.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.PrettyPrint
    ( Doc
    , toString
    , hPutDoc
    , putDoc

    , string
    , space
    , newline
    , indent

    , (<+>)
    , vcat

    , bold
    , underline

    , dullblack
    , dullred
    , dullgreen
    , dullyellow
    , dullblue
    , dullmagenta
    , dullcyan
    , dullwhite

    , ondullblack
    , ondullred
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader (asks, local)
import           Control.Monad.RWS    (RWS, runRWS)
import           Control.Monad.State  (get, modify)
import           Control.Monad.Writer (tell)
import           Data.List            (intersperse)
import           Data.Monoid          ((<>))
import           Data.String          (IsString (..))
import qualified System.Console.ANSI  as Ansi
import qualified System.IO            as IO


--------------------------------------------------------------------------------
-- | A simple chunk of text.  All ANSI codes are "reset" after printing.
data Chunk
    = StringChunk [Ansi.SGR] String
    | NewlineChunk


--------------------------------------------------------------------------------
hPutChunk :: IO.Handle -> Chunk -> IO ()
hPutChunk h NewlineChunk            = IO.hPutStrLn h ""
hPutChunk h (StringChunk codes str) = do
    Ansi.hSetSGR h (reverse codes)
    IO.hPutStr h str
    Ansi.hSetSGR h [Ansi.Reset]


--------------------------------------------------------------------------------
chunkToString :: Chunk -> String
chunkToString NewlineChunk        = "\n"
chunkToString (StringChunk _ str) = str


--------------------------------------------------------------------------------
data DocE
    = String String
    | Space
    | Newline
    | Ansi
        { ansiCode :: Ansi.SGR
        , ansiDoc  :: Doc
        }
    | Indent
        { indentFirstLine  :: Doc
        , indentOtherLines :: Doc
        , indentDoc        :: Doc
        }


--------------------------------------------------------------------------------
newtype Doc = Doc {unDoc :: [DocE]}
    deriving (Monoid)


--------------------------------------------------------------------------------
instance IsString Doc where
    fromString = string


--------------------------------------------------------------------------------
instance Show Doc where
    show = toString


--------------------------------------------------------------------------------
data DocEnv = DocEnv
    { deCodes  :: [Ansi.SGR]  -- ^ Most recent ones first in the list
    , deIndent :: [Chunk]     -- ^ Don't need to store first-line indent
    }


--------------------------------------------------------------------------------
type DocM = RWS DocEnv [Chunk] LineBuffer


--------------------------------------------------------------------------------
data LineBuffer
    = IndentOnly    [Chunk]
    | ActualContent [Chunk]


--------------------------------------------------------------------------------
instance Monoid LineBuffer where
    mempty = IndentOnly mempty

    mappend (IndentOnly    x) (IndentOnly    y) = IndentOnly    (x <> y)
    mappend (IndentOnly    x) (ActualContent y) = ActualContent (x <> y)
    mappend (ActualContent x) (ActualContent y) = ActualContent (x <> y)
    mappend (ActualContent x) (IndentOnly    y) = ActualContent (x <> y)


--------------------------------------------------------------------------------
bufferToChunks :: LineBuffer -> [Chunk]
bufferToChunks (IndentOnly    _) = []
bufferToChunks (ActualContent c) = c


--------------------------------------------------------------------------------
docToChunks :: Doc -> [Chunk]
docToChunks doc0 =
    let env0        = DocEnv [] []
        ((), b, cs) = runRWS (mapM_ go $ unDoc doc0) env0 mempty in
    cs <> bufferToChunks b
  where
    go :: DocE -> DocM ()

    go (String str) = do
        chunk <- makeChunk str
        modify (<> ActualContent [chunk])

    go Space = do
        chunk <- makeChunk " "
        modify (<> ActualContent [chunk])

    go Newline = do
        buffer <- get
        tell $ bufferToChunks buffer <> [NewlineChunk]
        indentation <- asks deIndent
        modify $ \_ -> IndentOnly indentation

    go Ansi {..} = do
        local (\env -> env {deCodes = ansiCode : deCodes env}) $
            mapM_ go (unDoc ansiDoc)

    go Indent {..} = do
        let firstLine  = docToChunks indentFirstLine
            otherLines = docToChunks indentOtherLines
        local (\env -> env {deIndent = deIndent env <> otherLines}) $ do
            modify (<> ActualContent firstLine)
            mapM_ go (unDoc indentDoc)

    makeChunk :: String -> DocM Chunk
    makeChunk str = do
        codes <- asks deCodes
        return $ StringChunk codes str


--------------------------------------------------------------------------------
toString :: Doc -> String
toString = concat . map chunkToString . docToChunks


--------------------------------------------------------------------------------
hPutDoc :: IO.Handle -> Doc -> IO ()
hPutDoc h = mapM_ (hPutChunk h) . docToChunks


--------------------------------------------------------------------------------
putDoc :: Doc -> IO ()
putDoc = hPutDoc IO.stdout


--------------------------------------------------------------------------------
mkDoc :: DocE -> Doc
mkDoc e = Doc [e]


--------------------------------------------------------------------------------
string :: String -> Doc
string = mkDoc . String  -- TODO (jaspervdj): Newline conversion


--------------------------------------------------------------------------------
space :: Doc
space = mkDoc Space


--------------------------------------------------------------------------------
newline :: Doc
newline = mkDoc Newline


--------------------------------------------------------------------------------
indent :: Doc -> Doc -> Doc -> Doc
indent indentFirstLine indentOtherLines indentDoc = mkDoc Indent {..}


--------------------------------------------------------------------------------
(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y
infixr 6 <+>


--------------------------------------------------------------------------------
vcat :: [Doc] -> Doc
vcat = mconcat . intersperse newline


--------------------------------------------------------------------------------
bold :: Doc -> Doc
bold = mkDoc . Ansi (Ansi.SetConsoleIntensity Ansi.BoldIntensity)


--------------------------------------------------------------------------------
underline :: Doc -> Doc
underline = mkDoc . Ansi (Ansi.SetUnderlining Ansi.SingleUnderline)


--------------------------------------------------------------------------------
dullcolor :: Ansi.Color -> Doc -> Doc
dullcolor c = mkDoc . Ansi (Ansi.SetColor Ansi.Foreground Ansi.Dull c)


--------------------------------------------------------------------------------
dullblack, dullred, dullgreen, dullyellow, dullblue, dullmagenta, dullcyan,
    dullwhite :: Doc -> Doc
dullblack   = dullcolor Ansi.Black
dullred     = dullcolor Ansi.Red
dullgreen   = dullcolor Ansi.Green
dullyellow  = dullcolor Ansi.Yellow
dullblue    = dullcolor Ansi.Blue
dullmagenta = dullcolor Ansi.Magenta
dullcyan    = dullcolor Ansi.Cyan
dullwhite   = dullcolor Ansi.White


--------------------------------------------------------------------------------
ondullcolor :: Ansi.Color -> Doc -> Doc
ondullcolor c = mkDoc . Ansi (Ansi.SetColor Ansi.Background Ansi.Dull c)


--------------------------------------------------------------------------------
ondullblack :: Doc -> Doc
ondullblack = ondullcolor Ansi.Black


--------------------------------------------------------------------------------
ondullred :: Doc -> Doc
ondullred = ondullcolor Ansi.Red
