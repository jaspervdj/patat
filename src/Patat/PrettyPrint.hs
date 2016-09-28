--------------------------------------------------------------------------------
-- | This is a small pretty-printing library.
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
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

    , Trimmable (..)
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
import           Control.Monad        (unless)
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
        { indentFirstLine  :: LineBuffer
        , indentOtherLines :: LineBuffer
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
    , deIndent :: LineBuffer  -- ^ Don't need to store first-line indent
    }


--------------------------------------------------------------------------------
type DocM = RWS DocEnv [Chunk] LineBuffer


--------------------------------------------------------------------------------
data Trimmable a
    = NotTrimmable !a
    | Trimmable    !a
    deriving (Foldable, Functor, Traversable)


--------------------------------------------------------------------------------
-- | Note that this is reversed so we have fast append
type LineBuffer = [Trimmable Chunk]


--------------------------------------------------------------------------------
bufferToChunks :: LineBuffer -> [Chunk]
bufferToChunks = map trimmableToChunk . reverse . dropWhile isTrimmable
  where
    isTrimmable (NotTrimmable _) = False
    isTrimmable (Trimmable    _) = True

    trimmableToChunk (NotTrimmable c) = c
    trimmableToChunk (Trimmable    c) = c


--------------------------------------------------------------------------------
docToChunks :: Doc -> [Chunk]
docToChunks doc0 =
    let env0        = DocEnv [] []
        ((), b, cs) = runRWS (go $ unDoc doc0) env0 mempty in
    cs <> bufferToChunks b
  where
    go :: [DocE] -> DocM ()

    go [] = return ()

    go (String str : docs) = do
        chunk <- makeChunk str
        modify (NotTrimmable chunk :)
        go docs

    go (Space : docs) = do
        chunk <- makeChunk " "
        modify (NotTrimmable chunk :)
        go docs

    go (Newline : docs) = do
        buffer <- get
        tell $ bufferToChunks buffer <> [NewlineChunk]
        indentation <- asks deIndent
        modify $ \_ -> if null docs then [] else indentation
        go docs

    go (Ansi {..} : docs) = do
        local (\env -> env {deCodes = ansiCode : deCodes env}) $
            go (unDoc ansiDoc)
        go docs

    go (Indent {..} : docs) = do
        local (\env -> env {deIndent = indentOtherLines ++ deIndent env}) $ do
            modify (indentFirstLine ++)
            go (unDoc indentDoc)
        go docs

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
indent :: Trimmable Doc -> Trimmable Doc -> Doc -> Doc
indent firstLineDoc otherLinesDoc doc = mkDoc $ Indent
    { indentFirstLine  = traverse docToChunks firstLineDoc
    , indentOtherLines = traverse docToChunks otherLinesDoc
    , indentDoc        = doc
    }


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
