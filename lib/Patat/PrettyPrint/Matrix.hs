--------------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Patat.PrettyPrint.Matrix
    ( Matrix
    , Cell (..)
    , emptyCell
    , docToMatrix
    , hPutMatrix
    ) where


--------------------------------------------------------------------------------
import           Control.Monad              (unless, when)
import           Data.Char.WCWidth.Extended (wcwidth)
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as VM
import           Patat.PrettyPrint.Internal hiding (null)
import           Patat.Size                 (Size (..))
import qualified System.Console.ANSI        as Ansi
import qualified System.IO                  as IO


--------------------------------------------------------------------------------
data Cell = Cell [Ansi.SGR] Char deriving (Eq, Show)


--------------------------------------------------------------------------------
type Matrix = V.Vector Cell


--------------------------------------------------------------------------------
emptyCell :: Cell
emptyCell = Cell [] ' '


--------------------------------------------------------------------------------
docToMatrix :: Size -> Doc -> Matrix
docToMatrix size doc = V.create $ do
    matrix <- VM.replicate (sRows size * sCols size) emptyCell
    go matrix 0 0 $ docToChunks doc
    pure matrix
  where
    go _ _ _ []                                      = pure ()
    go _ y _ _  | y >= sRows size                    = pure ()
    go r y _ (NewlineChunk : cs)                     = go r (y + 1) 0 cs
    go r y x cs | x > sCols size                     = go r (y + 1) 0 cs
    go r y x (ControlChunk ClearScreenControl  : cs) = go r y x cs  -- ?
    go r _ x (ControlChunk (GoToLineControl y) : cs) = go r y x cs
    go r y x (StringChunk _      []      : cs)       = go r y x cs
    go r y x (StringChunk codes (z : zs) : cs)       = do
        VM.write r (y * sCols size + x) (Cell codes z)
        go r y (x + wcwidth z) (StringChunk codes zs : cs)


--------------------------------------------------------------------------------
hPutMatrix :: IO.Handle -> Size -> Matrix -> IO ()
hPutMatrix h size matrix = go 0 0 0 []
  where
    go !y !x !empties prevCodes
        | x >= sCols size     = IO.hPutStrLn h "" >> go (y + 1) 0 0 prevCodes
        | y >= sRows size     = Ansi.hSetSGR h [Ansi.Reset]
        -- Try to not print empty things (e.g. fill the screen with spaces) as
        -- an optimization.  Instead, store the number of empties and print them
        -- when something actually follows.
        | cell == emptyCell   = do
            unless (null prevCodes) $ Ansi.hSetSGR h [Ansi.Reset]
            go y (x + 1) (empties + 1) []
        | otherwise           = do
            unless (empties == 0) $ IO.hPutStr h (replicate empties ' ')
            when (prevCodes /= codes) $
                Ansi.hSetSGR h (Ansi.Reset : reverse codes)
            IO.hPutStr h [c]
            go y (x + wcwidth c) 0 codes
      where
        cell@(Cell codes c) = matrix V.! (y * sCols size + x)
