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
    , dimensions
    , null

    , hPutDoc
    , putDoc

    , string
    , space
    , newline

    , Trimmable (..)
    , indent

    , (<+>)
    , (<$$>)
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

    -- * Exotic combinators
    , Alignment (..)
    , align
    , paste
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader (asks, local)
import           Control.Monad.RWS    (RWS, runRWS)
import           Control.Monad.State  (get, modify)
import           Control.Monad.Writer (tell)
import           Data.Foldable        (Foldable)
import qualified Data.List            as L
import           Data.Monoid          (mconcat, (<>))
import           Data.String          (IsString (..))
import           Data.Traversable     (traverse)
import qualified System.Console.ANSI  as Ansi
import qualified System.IO            as IO
import           Prelude              hiding (null)


--------------------------------------------------------------------------------
-- | A simple chunk of text.  All ANSI codes are "reset" after printing.
data Chunk
    = StringChunk [Ansi.SGR] String
    | NewlineChunk
    deriving (Eq)


--------------------------------------------------------------------------------
type Chunks = [Chunk]


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
-- | If two neighboring chunks have the same set of ANSI codes, we can group
-- them together.
optimizeChunks :: Chunks -> Chunks
optimizeChunks (StringChunk c1 s1 : StringChunk c2 s2 : chunks)
    | c1 == c2  = optimizeChunks (StringChunk c1 (s1 <> s2) : chunks)
    | otherwise =
        StringChunk c1 s1 : optimizeChunks (StringChunk c2 s2 : chunks)
optimizeChunks (x : chunks) = x : optimizeChunks chunks
optimizeChunks [] = []


--------------------------------------------------------------------------------
chunkLines :: Chunks -> [Chunks]
chunkLines chunks = case break (== NewlineChunk) chunks of
    (xs, _newline : ys) -> xs : chunkLines ys
    (xs, [])            -> [xs]


--------------------------------------------------------------------------------
data DocE
    = String String
    | Space
    | Newline
    | Ansi
        { ansiCode :: [Ansi.SGR] -> [Ansi.SGR]  -- ^ Modifies current codes.
        , ansiDoc  :: Doc
        }
    | Indent
        { indentFirstLine  :: LineBuffer
        , indentOtherLines :: LineBuffer
        , indentDoc        :: Doc
        }


--------------------------------------------------------------------------------
chunkToDocE :: Chunk -> DocE
chunkToDocE NewlineChunk            = Newline
chunkToDocE (StringChunk codes str) = Ansi (\_ -> codes) (Doc [String str])


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
type DocM = RWS DocEnv Chunks LineBuffer


--------------------------------------------------------------------------------
data Trimmable a
    = NotTrimmable !a
    | Trimmable    !a
    deriving (Foldable, Functor, Traversable)


--------------------------------------------------------------------------------
-- | Note that this is reversed so we have fast append
type LineBuffer = [Trimmable Chunk]


--------------------------------------------------------------------------------
bufferToChunks :: LineBuffer -> Chunks
bufferToChunks = map trimmableToChunk . reverse . dropWhile isTrimmable
  where
    isTrimmable (NotTrimmable _) = False
    isTrimmable (Trimmable    _) = True

    trimmableToChunk (NotTrimmable c) = c
    trimmableToChunk (Trimmable    c) = c


--------------------------------------------------------------------------------
docToChunks :: Doc -> Chunks
docToChunks doc0 =
    let env0        = DocEnv [] []
        ((), b, cs) = runRWS (go $ unDoc doc0) env0 mempty in
    optimizeChunks (cs <> bufferToChunks b)
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
        modify $ \_ -> if L.null docs then [] else indentation
        go docs

    go (Ansi {..} : docs) = do
        local (\env -> env {deCodes = ansiCode (deCodes env)}) $
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
-- | Returns the rows and columns necessary to render this document
dimensions :: Doc -> (Int, Int)
dimensions doc =
    let ls = lines (toString doc) in
    (length ls, foldr max 0 (map length ls))


--------------------------------------------------------------------------------
null :: Doc -> Bool
null doc = case unDoc doc of [] -> True; _ -> False


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
(<$$>) :: Doc -> Doc -> Doc
x <$$> y = x <> newline <> y
infixr 5 <$$>


--------------------------------------------------------------------------------
vcat :: [Doc] -> Doc
vcat = mconcat . L.intersperse newline


--------------------------------------------------------------------------------
bold :: Doc -> Doc
bold = mkDoc . Ansi
    (\codes -> Ansi.SetConsoleIntensity Ansi.BoldIntensity : codes)


--------------------------------------------------------------------------------
underline :: Doc -> Doc
underline = mkDoc . Ansi
    (\codes -> Ansi.SetUnderlining Ansi.SingleUnderline : codes)


--------------------------------------------------------------------------------
dullcolor :: Ansi.Color -> Doc -> Doc
dullcolor c = mkDoc . Ansi
    (\codes -> Ansi.SetColor Ansi.Foreground Ansi.Dull c : codes)


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
ondullcolor c = mkDoc . Ansi
    (\codes -> Ansi.SetColor Ansi.Background Ansi.Dull c : codes)


--------------------------------------------------------------------------------
ondullblack :: Doc -> Doc
ondullblack = ondullcolor Ansi.Black


--------------------------------------------------------------------------------
ondullred :: Doc -> Doc
ondullred = ondullcolor Ansi.Red


--------------------------------------------------------------------------------
data Alignment = AlignLeft | AlignCenter | AlignRight deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
align :: Int -> Alignment -> Doc -> Doc
align width alignment doc0 =
    let chunks0 = docToChunks doc0
        lines_  = chunkLines chunks0 in
    vcat
        [ Doc (map chunkToDocE (alignLine line))
        | line <- lines_
        ]
  where
    lineWidth :: [Chunk] -> Int
    lineWidth = sum . map (length . chunkToString)

    alignLine :: [Chunk] -> [Chunk]
    alignLine line =
        let actual   = lineWidth line
            spaces n = [StringChunk [] (replicate n ' ')] in
        case alignment of
            AlignLeft   -> line <> spaces (width - actual)
            AlignRight  -> spaces (width - actual) <> line
            AlignCenter ->
                let r = (width - actual) `div` 2
                    l = (width - actual) - r in
                spaces l <> line <> spaces r


--------------------------------------------------------------------------------
-- | Like the unix program 'paste'.
paste :: [Doc] -> Doc
paste docs0 =
    let chunkss = map docToChunks docs0                   :: [Chunks]
        cols    = map chunkLines chunkss                  :: [[Chunks]]
        rows0   = L.transpose cols                        :: [[Chunks]]
        rows1   = map (map (Doc . map chunkToDocE)) rows0 :: [[Doc]] in
    vcat $ map mconcat rows1
