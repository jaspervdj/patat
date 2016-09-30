--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Presentation.Display.Table
    ( Table (..)
    , prettyTable
    ) where


--------------------------------------------------------------------------------
import           Data.List         (intersperse, transpose)
import           Data.Monoid       ((<>))
import           Patat.PrettyPrint ((<$$>))
import qualified Patat.PrettyPrint as PP
import qualified Text.Pandoc       as Pandoc


--------------------------------------------------------------------------------
data Table = Table
    { tCaption :: [Pandoc.Inline]
    , tAligns  :: [Pandoc.Alignment]
    , tHeaders :: [Pandoc.TableCell]
    , tRows    :: [[Pandoc.TableCell]]
    }


--------------------------------------------------------------------------------
prettyTable
    :: ([Pandoc.Block]  -> PP.Doc)
    -> ([Pandoc.Inline] -> PP.Doc)
    -> Table -> PP.Doc
prettyTable prettyBlocks prettyInlines Table {..}
    | all (all isSimpleCell) (tHeaders : tRows) =
        hcat2
            [ PP.dullblue (prettySimpleCell w a header)
            | (w, a, header) <- zip3 columnWidths tAligns tHeaders
            ] <$$>
        hcat2
            [ PP.dullmagenta (PP.string (replicate w '-'))
            | w <- columnWidths
            ] <$$>
        PP.vcat
            [ hcat2
                [ prettySimpleCell w a cell
                | (w, a, cell) <- zip3 columnWidths tAligns row
                ]
            | row <- tRows
            ] <$$>
        (case tCaption of
            [] -> mempty
            _  -> PP.newline <> "Table: " <> prettyInlines tCaption)
    | otherwise                               = undefined
  where
    isSimpleCell [Pandoc.Plain _] = True
    isSimpleCell _                = False

    hcat2 :: [PP.Doc] -> PP.Doc
    hcat2 = mconcat . intersperse (PP.string "  ")

    prettySimpleCell :: Int -> Pandoc.Alignment -> Pandoc.TableCell -> PP.Doc
    prettySimpleCell width align blocks = case align of
        Pandoc.AlignLeft    -> doc <> spaces (width - actual)
        Pandoc.AlignDefault -> doc <> spaces (width - actual)
        Pandoc.AlignRight   -> spaces (width - actual) <> doc
        Pandoc.AlignCenter  ->
            let r = (width - actual) `div` 2
                l = width - actual - r in
            spaces l <> doc <> spaces r
      where
        doc         = prettyBlocks blocks
        (_, actual) = PP.dimensions doc
        spaces n    = PP.string (replicate n ' ')

    rowDimensions :: [[(Int, Int)]]
    rowDimensions = map (map (PP.dimensions . prettyBlocks)) (tHeaders : tRows)

    columnWidths :: [Int]
    columnWidths =
        [ foldr max 0 (map snd col)
        | col <- transpose rowDimensions
        ]
