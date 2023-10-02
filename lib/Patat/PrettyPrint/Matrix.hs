--------------------------------------------------------------------------------
module Patat.PrettyPrint.Matrix
    ( Matrix
    , Elem (..)
    , docToMatrix
    , hPutMatrix
    ) where


--------------------------------------------------------------------------------
import           Control.Monad              (when)
import           Data.Char.WCWidth.Extended (wcwidth)
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as VM
import           Patat.Presentation.Display (Size (..))
import           Patat.PrettyPrint.Internal
import qualified System.Console.ANSI        as Ansi
import qualified System.IO                  as IO


--------------------------------------------------------------------------------
data Elem = Elem [Ansi.SGR] Char


--------------------------------------------------------------------------------
type Matrix = V.Vector Elem


--------------------------------------------------------------------------------
emptyElem :: Elem
emptyElem = Elem [] ' '


--------------------------------------------------------------------------------
docToMatrix :: Size -> Doc -> Matrix
docToMatrix size doc = V.create $ do
    matrix <- VM.replicate (sRows size * sCols size) emptyElem
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
        VM.write r (y * sCols size + x) (Elem codes z)
        go r y (x + wcwidth z) (StringChunk codes zs : cs)


--------------------------------------------------------------------------------
hPutMatrix :: IO.Handle -> Size -> Matrix -> IO ()
hPutMatrix h size matrix = go 0 0 []
  where
    go y x codes0
        | x >= sCols size     = IO.hPutStrLn h "" >> go (y + 1) 0 codes0
        -- | Never use the last line, it's empty to receive prompts.
        | y + 1 >= sRows size = Ansi.hSetSGR h [Ansi.Reset]
        | otherwise           = do
            when (codes0 /= codes1) $
                Ansi.hSetSGR h (Ansi.Reset : reverse codes1)
            IO.hPutStr h [c]
            go y (x + wcwidth c) codes1
      where
          Elem codes1 c = matrix V.! (y * sCols size + x)
