import Goldplate   (Options (..), defaultOptions, mainWith)
import System.Exit (exitWith)

main :: IO ()
main = mainWith options >>= exitWith
  where
    options = defaultOptions
        { oPrettyDiff = True, oPaths = ["tests/golden"] }
