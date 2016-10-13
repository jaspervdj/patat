--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Presentation.Display.Table
    ( Table (..)
    , prettyTable

    , themed
    ) where


--------------------------------------------------------------------------------
import           Data.List         (intersperse, transpose)
import           Data.Monoid       (mconcat, mempty, (<>))
import           Patat.PrettyPrint ((<$$>))
import qualified Patat.PrettyPrint as PP
import           Patat.Theme       (Theme (..))
import qualified Patat.Theme       as Theme
import           Prelude


--------------------------------------------------------------------------------
data Table = Table
    { tCaption :: PP.Doc
    , tAligns  :: [PP.Alignment]
    , tHeaders :: [PP.Doc]
    , tRows    :: [[PP.Doc]]
    }


--------------------------------------------------------------------------------
prettyTable
    :: Theme -> Table -> PP.Doc
prettyTable theme@Theme {..} Table {..} =
    PP.indent (PP.Trimmable "  ") (PP.Trimmable "  ") $
        lineIf (not isHeaderLess) (hcat2 headerHeight
            [ themed themeTableHeader (PP.align w a (vpad headerHeight header))
            | (w, a, header) <- zip3 columnWidths tAligns tHeaders
            ]) <>
        dashedHeaderSeparator theme columnWidths <$$>
        joinRows
            [ hcat2 rowHeight
                [ PP.align w a (vpad rowHeight cell)
                | (w, a, cell) <- zip3 columnWidths tAligns row
                ]
            | (rowHeight, row) <- zip rowHeights tRows
            ] <$$>
        lineIf isHeaderLess (dashedHeaderSeparator theme columnWidths) <>
        lineIf
            (not $ PP.null tCaption) (PP.hardline <> "Table: " <> tCaption)
  where
    lineIf cond line = if cond then line <> PP.hardline else mempty

    joinRows
        | all (all isSimpleCell) tRows = PP.vcat
        | otherwise                    = PP.vcat . intersperse ""

    isHeaderLess = all PP.null tHeaders

    headerDimensions = map PP.dimensions tHeaders :: [(Int, Int)]
    rowDimensions    = map (map PP.dimensions) tRows :: [[(Int, Int)]]

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
dashedHeaderSeparator :: Theme -> [Int] -> PP.Doc
dashedHeaderSeparator Theme {..} columnWidths =
    mconcat $ intersperse (PP.string "  ")
        [ themed themeTableSeparator (PP.string (replicate w '-'))
        | w <- columnWidths
        ]


--------------------------------------------------------------------------------
-- | This does not really belong in the module.
themed :: Maybe Theme.Style -> PP.Doc -> PP.Doc
themed Nothing                    = id
themed (Just (Theme.Style []))    = id
themed (Just (Theme.Style codes)) = PP.ansi codes
