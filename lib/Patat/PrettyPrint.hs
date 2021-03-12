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
    , text
    , space
    , spaces
    , softline
    , hardline

    , wrapAt

    , Trimmable (..)
    , indent

    , ansi

    , (<+>)
    , (<$$>)
    , vcat
    , intersperse

    -- * Exotic combinators
    , Alignment (..)
    , align
    , paste

    -- * Control codes
    , removeControls
    , clearScreen
    , goToLine
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader       (asks, local)
import           Control.Monad.RWS          (RWS, runRWS)
import           Control.Monad.State        (get, gets, modify)
import           Control.Monad.Writer       (tell)
import           Data.Char.WCWidth.Extended (wcstrwidth)
import qualified Data.List                  as L
import           Data.String                (IsString (..))
import qualified Data.Text                  as T
import           Prelude                    hiding (null)
import qualified System.Console.ANSI        as Ansi
import qualified System.IO                  as IO


--------------------------------------------------------------------------------
-- | Control actions for the terminal.
data Control
    = ClearScreenControl
    | GoToLineControl Int
    deriving (Eq)


--------------------------------------------------------------------------------
-- | A simple chunk of text.  All ANSI codes are "reset" after printing.
data Chunk
    = StringChunk [Ansi.SGR] String
    | NewlineChunk
    | ControlChunk Control
    deriving (Eq)


--------------------------------------------------------------------------------
type Chunks = [Chunk]


--------------------------------------------------------------------------------
hPutChunk :: IO.Handle -> Chunk -> IO ()
hPutChunk h NewlineChunk = IO.hPutStrLn h ""
hPutChunk h (StringChunk codes str) = do
    Ansi.hSetSGR h (reverse codes)
    IO.hPutStr h str
    Ansi.hSetSGR h [Ansi.Reset]
hPutChunk h (ControlChunk ctrl) = case ctrl of
    ClearScreenControl -> Ansi.hClearScreen h
    GoToLineControl l  -> Ansi.hSetCursorPosition h l 0


--------------------------------------------------------------------------------
chunkToString :: Chunk -> String
chunkToString NewlineChunk        = "\n"
chunkToString (StringChunk _ str) = str
chunkToString (ControlChunk _)    = ""


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
data DocE d
    = String String
    | Softspace
    | Hardspace
    | Softline
    | Hardline
    | WrapAt
        { wrapAtCol :: Maybe Int
        , wrapDoc   :: d
        }
    | Ansi
        { ansiCode :: [Ansi.SGR] -> [Ansi.SGR]  -- ^ Modifies current codes.
        , ansiDoc  :: d
        }
    | Indent
        { indentFirstLine  :: LineBuffer
        , indentOtherLines :: LineBuffer
        , indentDoc        :: d
        }
    | Control Control
    deriving (Functor)


--------------------------------------------------------------------------------
chunkToDocE :: Chunk -> DocE Doc
chunkToDocE NewlineChunk            = Hardline
chunkToDocE (StringChunk codes str) = Ansi (\_ -> codes) (Doc [String str])
chunkToDocE (ControlChunk ctrl)     = Control ctrl


--------------------------------------------------------------------------------
newtype Doc = Doc {unDoc :: [DocE Doc]}
    deriving (Monoid, Semigroup)


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
    , deWrap   :: Maybe Int   -- ^ Wrap at columns
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
    let env0        = DocEnv [] [] Nothing
        ((), b, cs) = runRWS (go $ unDoc doc0) env0 mempty in
    optimizeChunks (cs <> bufferToChunks b)
  where
    go :: [DocE Doc] -> DocM ()

    go [] = return ()

    go (String str : docs) = do
        chunk <- makeChunk str
        modify (NotTrimmable chunk :)
        go docs

    go (Softspace : docs) = do
        hard <- softConversion Softspace docs
        go (hard : docs)

    go (Hardspace : docs) = do
        chunk <- makeChunk " "
        modify (NotTrimmable chunk :)
        go docs

    go (Softline : docs) = do
        hard <- softConversion Softline docs
        go (hard : docs)

    go (Hardline : docs) = do
        buffer <- get
        tell $ bufferToChunks buffer <> [NewlineChunk]
        indentation <- asks deIndent
        modify $ \_ -> if L.null docs then [] else indentation
        go docs

    go (WrapAt {..} : docs) = do
        local (\env -> env {deWrap = wrapAtCol}) $ go (unDoc wrapDoc)
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

    go (Control ctrl : docs) = do
        tell [ControlChunk ctrl]
        go docs


    makeChunk :: String -> DocM Chunk
    makeChunk str = do
        codes <- asks deCodes
        return $ StringChunk codes str

    -- Convert 'Softspace' or 'Softline' to 'Hardspace' or 'Hardline'
    softConversion :: DocE Doc -> [DocE Doc] -> DocM (DocE Doc)
    softConversion soft docs = do
        mbWrapCol <- asks deWrap
        case mbWrapCol of
            Nothing     -> return hard
            Just maxCol -> do
                -- Slow.
                currentLine <- gets (concatMap chunkToString . bufferToChunks)
                let currentCol = wcstrwidth currentLine
                case nextWordLength docs of
                    Nothing                            -> return hard
                    Just l
                        | currentCol + 1 + l <= maxCol -> return Hardspace
                        | otherwise                    -> return Hardline
      where
        hard = case soft of
            Softspace -> Hardspace
            Softline  -> Hardline
            _         -> soft

    nextWordLength :: [DocE Doc] -> Maybe Int
    nextWordLength []                 = Nothing
    nextWordLength (String x : xs)
        | L.null x                    = nextWordLength xs
        | otherwise                   = Just (wcstrwidth x)
    nextWordLength (Softspace : xs)   = nextWordLength xs
    nextWordLength (Hardspace : xs)   = nextWordLength xs
    nextWordLength (Softline : xs)    = nextWordLength xs
    nextWordLength (Hardline : _)     = Nothing
    nextWordLength (WrapAt {..} : xs) = nextWordLength (unDoc wrapDoc   ++ xs)
    nextWordLength (Ansi   {..} : xs) = nextWordLength (unDoc ansiDoc   ++ xs)
    nextWordLength (Indent {..} : xs) = nextWordLength (unDoc indentDoc ++ xs)
    nextWordLength (Control _ : _)    = Nothing


--------------------------------------------------------------------------------
toString :: Doc -> String
toString = concat . map chunkToString . docToChunks


--------------------------------------------------------------------------------
-- | Returns the rows and columns necessary to render this document
dimensions :: Doc -> (Int, Int)
dimensions doc =
    let ls = lines (toString doc) in
    (length ls, foldr max 0 (map wcstrwidth ls))


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
mkDoc :: DocE Doc -> Doc
mkDoc e = Doc [e]


--------------------------------------------------------------------------------
string :: String -> Doc
string = mkDoc . String  -- TODO (jaspervdj): Newline conversion


--------------------------------------------------------------------------------
text :: T.Text -> Doc
text = string . T.unpack


--------------------------------------------------------------------------------
space :: Doc
space = mkDoc Softspace


--------------------------------------------------------------------------------
spaces :: Int -> Doc
spaces n = mconcat $ replicate n space


--------------------------------------------------------------------------------
softline :: Doc
softline = mkDoc Softline


--------------------------------------------------------------------------------
hardline :: Doc
hardline = mkDoc Hardline


--------------------------------------------------------------------------------
wrapAt :: Maybe Int -> Doc -> Doc
wrapAt wrapAtCol wrapDoc = mkDoc WrapAt {..}


--------------------------------------------------------------------------------
indent :: Trimmable Doc -> Trimmable Doc -> Doc -> Doc
indent firstLineDoc otherLinesDoc doc = mkDoc $ Indent
    { indentFirstLine  = traverse docToChunks firstLineDoc
    , indentOtherLines = traverse docToChunks otherLinesDoc
    , indentDoc        = doc
    }


--------------------------------------------------------------------------------
ansi :: [Ansi.SGR] -> Doc -> Doc
ansi codes =  mkDoc . Ansi (codes ++)


--------------------------------------------------------------------------------
(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y
infixr 6 <+>


--------------------------------------------------------------------------------
(<$$>) :: Doc -> Doc -> Doc
x <$$> y = x <> hardline <> y
infixr 5 <$$>


--------------------------------------------------------------------------------
vcat :: [Doc] -> Doc
vcat = intersperse hardline


--------------------------------------------------------------------------------
intersperse :: Doc -> [Doc] -> Doc
intersperse sep = mconcat . L.intersperse sep


--------------------------------------------------------------------------------
data Alignment = AlignLeft | AlignCenter | AlignRight deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------
align :: Int -> Alignment -> Doc -> Doc
align width alignment doc0 =
    let chunks0 = docToChunks $ removeControls doc0
        lines_  = chunkLines chunks0 in
    vcat
        [ Doc (map chunkToDocE (alignLine line))
        | line <- lines_
        ]
  where
    lineWidth :: [Chunk] -> Int
    lineWidth = sum . map (wcstrwidth . chunkToString)

    alignLine :: [Chunk] -> [Chunk]
    alignLine line =
        let actual        = lineWidth line
            chunkSpaces n = [StringChunk [] (replicate n ' ')] in
        case alignment of
            AlignLeft   -> line <> chunkSpaces (width - actual)
            AlignRight  -> chunkSpaces (width - actual) <> line
            AlignCenter ->
                let r = (width - actual) `div` 2
                    l = (width - actual) - r in
                chunkSpaces l <> line <> chunkSpaces r


--------------------------------------------------------------------------------
-- | Like the unix program 'paste'.
paste :: [Doc] -> Doc
paste docs0 =
    let chunkss = map (docToChunks . removeControls) docs0 :: [Chunks]
        cols    = map chunkLines chunkss                   :: [[Chunks]]
        rows0   = L.transpose cols                         :: [[Chunks]]
        rows1   = map (map (Doc . map chunkToDocE)) rows0  :: [[Doc]] in
    vcat $ map mconcat rows1


--------------------------------------------------------------------------------
removeControls :: Doc -> Doc
removeControls = Doc . filter isNotControl . map (fmap removeControls) . unDoc
  where
    isNotControl (Control _) = False
    isNotControl _           = True


--------------------------------------------------------------------------------
clearScreen :: Doc
clearScreen = mkDoc $ Control ClearScreenControl


--------------------------------------------------------------------------------
goToLine :: Int -> Doc
goToLine = mkDoc . Control . GoToLineControl
