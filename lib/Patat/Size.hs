--------------------------------------------------------------------------------
module Patat.Size
    ( Size (..)
    , getTerminalSize
    ) where


--------------------------------------------------------------------------------
import           Data.Maybe                   (fromMaybe)
import qualified System.Console.Terminal.Size as Terminal


--------------------------------------------------------------------------------
data Size = Size {sRows :: Int, sCols :: Int} deriving (Show)


--------------------------------------------------------------------------------
getTerminalSize :: IO Size
getTerminalSize = do
    mbWindow <- Terminal.size
    let rows = fromMaybe 24 $ Terminal.height <$> mbWindow
        cols = fromMaybe 72 $ Terminal.width  <$> mbWindow
    pure $ Size {sRows = rows, sCols = cols}
