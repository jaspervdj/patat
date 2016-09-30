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
    { tCaption :: PP.Doc
    , tAligns  :: [PP.Alignment]
    , tHeaders :: [PP.Doc]
    , tRows    :: [[PP.Doc]]
    }


--------------------------------------------------------------------------------
prettyTable
    :: Table -> PP.Doc
prettyTable Table {..}
    | all (all isSimpleCell) (tHeaders : tRows) =
        PP.indent (PP.Trimmable "  ") (PP.Trimmable "  ") $
            lineIf (not isHeaderLess) (hcat2
                [ PP.dullblue (PP.align w a header)
                | (w, a, header) <- zip3 columnWidths tAligns tHeaders
                ]) <>
            headerSeparator <$$>
            PP.vcat
                [ hcat2
                    [ PP.align w a cell
                    | (w, a, cell) <- zip3 columnWidths tAligns row
                    ]
                | row <- tRows
                ] <$$>
            lineIf isHeaderLess headerSeparator <>
            (if PP.null tCaption
                then mempty
                else PP.newline <> "Table: " <> tCaption)
    | otherwise = PP.string $ show (tHeaders : tRows)
  where
    lineIf cond line = if cond then line <> PP.newline else mempty

    isHeaderLess = all PP.null tHeaders

    headerSeparator = hcat2
        [ PP.dullmagenta (PP.string (replicate w '-'))
        | w <- columnWidths
        ]

    isSimpleCell :: PP.Doc -> Bool
    isSimpleCell = (<= 1) . fst . PP.dimensions

    hcat2 :: [PP.Doc] -> PP.Doc
    hcat2 = mconcat . intersperse (PP.string "  ")

    rowDimensions :: [[(Int, Int)]]
    rowDimensions = map (map PP.dimensions) (tHeaders : tRows)

    columnWidths :: [Int]
    columnWidths =
        [ foldr max 0 (map snd col)
        | col <- transpose rowDimensions
        ]
