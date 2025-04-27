--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Presentation.Display.Table
    ( TableDisplay (..)
    , prettyTableDisplay

    , themed
    ) where


--------------------------------------------------------------------------------
import           Data.List                           (intersperse, transpose)
import           Patat.Presentation.Display.Internal
import           Patat.PrettyPrint                   ((<$$>))
import qualified Patat.PrettyPrint                   as PP
import           Patat.Theme                         (Theme (..))
import           Prelude


--------------------------------------------------------------------------------
data TableDisplay = TableDisplay
    { tdCaption :: PP.Doc
    , tdAligns  :: [PP.Alignment]
    , tdHeaders :: [PP.Doc]
    , tdRows    :: [[PP.Doc]]
    }


--------------------------------------------------------------------------------
prettyTableDisplay :: DisplaySettings -> TableDisplay -> PP.Doc
prettyTableDisplay ds TableDisplay {..} =
    PP.indent indentation indentation $
        lineIf (not isHeaderLess) (hcat2 headerHeight
            [ themed ds themeTableHeader $
                PP.align w a (vpad headerHeight header)
            | (w, a, header) <- zip3 columnWidths tdAligns tdHeaders
            ]) <>
        dashedHeaderSeparator ds columnWidths <$$>
        joinRows
            [ hcat2 rowHeight
                [ PP.align w a (vpad rowHeight cell)
                | (w, a, cell) <- zip3 columnWidths tdAligns row
                ]
            | (rowHeight, row) <- zip rowHeights tdRows
            ] <$$>
        lineIf isHeaderLess (dashedHeaderSeparator ds columnWidths) <>
        lineIf
            (not $ PP.null tdCaption) (PP.hardline <> "Table: " <> tdCaption)
  where
    indentation = PP.Indentation 2 mempty

    lineIf cond line = if cond then line <> PP.hardline else mempty

    joinRows
        | all (all isSimpleCell) tdRows = PP.vcat
        | otherwise                    = PP.vcat . intersperse ""

    isHeaderLess = all PP.null tdHeaders

    headerDimensions = map PP.dimensions tdHeaders :: [(Int, Int)]
    rowDimensions    = map (map PP.dimensions) tdRows :: [[(Int, Int)]]

    columnWidths :: [Int]
    columnWidths =
        [ safeMax (map snd col)
        | col <- transpose (headerDimensions : rowDimensions)
        ]

    rowHeights   = map (safeMax . map fst) rowDimensions :: [Int]
    headerHeight = safeMax (map fst headerDimensions)    :: Int

    vpad :: Int -> PP.Doc -> PP.Doc
    vpad height doc =
        let (actual, _) = PP.dimensions doc in
        doc <> mconcat (replicate (height - actual) PP.hardline)

    safeMax = foldr max 0

    hcat2 :: Int -> [PP.Doc] -> PP.Doc
    hcat2 rowHeight = PP.paste . intersperse (spaces2 rowHeight)

    spaces2 :: Int -> PP.Doc
    spaces2 rowHeight =
        mconcat $ intersperse PP.hardline $
        replicate rowHeight (PP.string "  ")


--------------------------------------------------------------------------------
isSimpleCell :: PP.Doc -> Bool
isSimpleCell = (<= 1) . fst . PP.dimensions


--------------------------------------------------------------------------------
dashedHeaderSeparator :: DisplaySettings -> [Int] -> PP.Doc
dashedHeaderSeparator ds columnWidths =
    mconcat $ intersperse (PP.string "  ")
        [ themed ds themeTableSeparator (PP.string (replicate w '-'))
        | w <- columnWidths
        ]
