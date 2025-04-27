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

    , char
    , string
    , text
    , space
    , spaces
    , softline
    , hardline

    , wrapAt

    , Indentation (..)
    , indent
    , deindent

    , ansi
    , hyperlink

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
import           Data.Char.WCWidth.Extended (wcstrwidth)
import qualified Data.List                  as L
import qualified Data.Text                  as T
import           Patat.PrettyPrint.Internal
import           Prelude                    hiding (null)
import qualified System.Console.ANSI        as Ansi


--------------------------------------------------------------------------------
char :: Char -> Doc
char = string . pure


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
indent :: Indentation Doc -> Indentation Doc -> Doc -> Doc
indent firstLineDoc otherLinesDoc doc = mkDoc $ Indent
    { indentFirstLine  = fmap docToChunks firstLineDoc
    , indentOtherLines = fmap docToChunks otherLinesDoc
    , indentDoc        = doc
    }


--------------------------------------------------------------------------------
-- | Only strips leading spaces
deindent :: Doc -> Doc
deindent = Doc . concatMap go . unDoc
  where
    go :: DocE Doc -> [DocE Doc]
    go doc@(Indent {..})
        | fs0 <= 0 && os0 <= 0 = [doc]
        | fs1 == 0 && os1 == 0 && L.null fc && L.null oc =
            concatMap go $ unDoc indentDoc
        | otherwise = pure $ Indent
            { indentFirstLine  = Indentation fs1 fc
            , indentOtherLines = Indentation os1 oc
            , indentDoc        = indentDoc
            }
      where
        Indentation fs0 fc = indentFirstLine
        Indentation os0 oc = indentOtherLines
        fs1 = fs0 - min fs0 os0
        os1 = os0 - min fs0 os0
    go doc = [doc]


--------------------------------------------------------------------------------
ansi :: [Ansi.SGR] -> Doc -> Doc
ansi codes =  mkDoc . Ansi (codes ++)


--------------------------------------------------------------------------------
hyperlink :: String -> Doc -> Doc
hyperlink url =  mkDoc . Hyperlink url


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
