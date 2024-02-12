--------------------------------------------------------------------------------
-- | This is a small pretty-printing library.
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.PrettyPrint.Internal
    ( Control (..)
    , Chunk (..)
    , Chunks
    , chunkToString
    , chunkLines

    , DocE (..)
    , chunkToDocE

    , Indentation (..)

    , Doc (..)
    , docToChunks

    , toString
    , dimensions
    , null

    , hPutDoc
    , putDoc
    , mkDoc
    , string
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Reader       (asks, local)
import           Control.Monad.RWS          (RWS, runRWS)
import           Control.Monad.State        (get, modify)
import           Control.Monad.Writer       (tell)
import           Data.Char.WCWidth.Extended (wcstrwidth)
import qualified Data.List                  as L
import           Data.String                (IsString (..))
import           Prelude                    hiding (null)
import qualified System.Console.ANSI        as Ansi
import qualified System.IO                  as IO


--------------------------------------------------------------------------------
-- | Control actions for the terminal.
data Control
    = ClearScreenControl
    | GoToLineControl Int
    deriving (Eq, Show)


--------------------------------------------------------------------------------
-- | A simple chunk of text.  All ANSI codes are "reset" after printing.
data Chunk
    = StringChunk [Ansi.SGR] String
    | NewlineChunk
    | ControlChunk Control
    deriving (Eq, Show)


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
        { indentFirstLine  :: Indentation [Chunk]
        , indentOtherLines :: Indentation [Chunk]
        , indentDoc        :: d
        }
    | Control Control
    deriving (Functor)


--------------------------------------------------------------------------------
chunkToDocE :: Chunk -> DocE Doc
chunkToDocE NewlineChunk         = Hardline
chunkToDocE (StringChunk c1 str) = Ansi (\c0 -> c1 ++ c0) (Doc [String str])
chunkToDocE (ControlChunk ctrl)  = Control ctrl


--------------------------------------------------------------------------------
newtype Doc = Doc {unDoc :: [DocE Doc]}
    deriving (Monoid, Semigroup)


--------------------------------------------------------------------------------
instance Show Doc where
    show = toString


--------------------------------------------------------------------------------
instance IsString Doc where
    fromString = string


--------------------------------------------------------------------------------
data DocEnv = DocEnv
    { deCodes  :: [Ansi.SGR]             -- ^ Most recent ones first in the list
    , deIndent :: [Indentation [Chunk]]  -- ^ No need to store first-line indent
    , deWrap   :: Maybe Int              -- ^ Wrap at columns
    }


--------------------------------------------------------------------------------
type DocM = RWS DocEnv Chunks LineBuffer


--------------------------------------------------------------------------------
-- | Note that the lists here are reversed so we have fast append.
-- We also store the current length to avoid having to recompute it.
data LineBuffer = LineBuffer Int [Indentation [Chunk]] [Chunk]


--------------------------------------------------------------------------------
emptyLineBuffer :: LineBuffer
emptyLineBuffer = LineBuffer 0 [] []


--------------------------------------------------------------------------------
data Indentation a = Indentation Int a
    deriving (Foldable, Functor, Traversable)


--------------------------------------------------------------------------------
indentationToChunks :: Indentation [Chunk] -> [Chunk]
indentationToChunks (Indentation 0 c) = c
indentationToChunks (Indentation n c) = StringChunk [] (replicate n ' ') : c


--------------------------------------------------------------------------------
indentationWidth :: Indentation [Chunk] -> Int
indentationWidth (Indentation s c) =
    s + sum (map (wcstrwidth . chunkToString) c)


--------------------------------------------------------------------------------
bufferToChunks :: LineBuffer -> Chunks
bufferToChunks (LineBuffer _ ind chunks) = case chunks of
    [] -> concatMap indentationToChunks $ reverse $
        dropWhile emptyIndentation ind
    _ -> concatMap indentationToChunks (reverse ind) ++ reverse chunks
  where
    emptyIndentation (Indentation _ []) = True
    emptyIndentation _                  = False


--------------------------------------------------------------------------------
docToChunks :: Doc -> Chunks
docToChunks doc0 =
    let env0        = DocEnv [] [] Nothing
        ((), b, cs) = runRWS (go $ unDoc doc0) env0 emptyLineBuffer in
    optimizeChunks (cs <> bufferToChunks b)
  where
    go :: [DocE Doc] -> DocM ()

    go [] = return ()

    go (String str : docs) = do
        chunk <- makeChunk str
        appendChunk chunk
        go docs

    go (Softspace : docs) = do
        hard <- softConversion Softspace docs
        go (hard : docs)

    go (Hardspace : docs) = do
        chunk <- makeChunk " "
        appendChunk chunk
        go docs

    go (Softline : docs) = do
        hard <- softConversion Softline docs
        go (hard : docs)

    go (Hardline : docs) = do
        buffer <- get
        tell $ bufferToChunks buffer <> [NewlineChunk]
        ind <- asks deIndent
        modify $ \_ -> case docs of
            []    -> emptyLineBuffer
            _ : _ -> LineBuffer (sum $ map indentationWidth ind) ind []
        go docs

    go (WrapAt {..} : docs) = do
        il <- asks $ sum . map indentationWidth . deIndent
        local (\env -> env {deWrap = fmap (+ il) wrapAtCol}) $ go (unDoc wrapDoc)
        go docs

    go (Ansi {..} : docs) = do
        local (\env -> env {deCodes = ansiCode (deCodes env)}) $
            go (unDoc ansiDoc)
        go docs

    go (Indent {..} : docs) = do
        local (\e -> e {deIndent = indentOtherLines : deIndent e}) $ do
            modify $ \(LineBuffer w i c) -> LineBuffer
                (w + indentationWidth indentFirstLine) (indentFirstLine : i) c
            go (unDoc indentDoc)
        go docs

    go (Control ctrl : docs) = do
        tell [ControlChunk ctrl]
        go docs


    makeChunk :: String -> DocM Chunk
    makeChunk str = do
        codes <- asks deCodes
        return $ StringChunk codes str

    appendChunk :: Chunk -> DocM ()
    appendChunk c = modify $ \(LineBuffer w i cs) ->
        LineBuffer (w + wcstrwidth (chunkToString c)) i (c : cs)

    -- Convert 'Softspace' or 'Softline' to 'Hardspace' or 'Hardline'
    softConversion :: DocE Doc -> [DocE Doc] -> DocM (DocE Doc)
    softConversion soft docs = do
        mbWrapCol <- asks deWrap
        case mbWrapCol of
            Nothing     -> return hard
            Just maxCol -> do
                LineBuffer currentCol _ _ <- get
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
string ""  = Doc []
string str = mkDoc $ String str  -- TODO (jaspervdj): Newline conversion?
