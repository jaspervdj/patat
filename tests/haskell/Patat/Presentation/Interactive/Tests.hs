module Patat.Presentation.Interactive.Tests
    ( tests
    ) where

import           Control.Monad                  (forM_, replicateM)
import           Patat.Presentation.Interactive
import           System.Directory               (getTemporaryDirectory,
                                                 removeFile)
import qualified System.IO                      as IO
import qualified Test.QuickCheck                as QC
import qualified Test.QuickCheck.Monadic        as QC
import qualified Test.Tasty                     as Tasty
import qualified Test.Tasty.QuickCheck                     as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Patat.Presentation.Interactive.Tests"
    [ Tasty.testProperty "testReadPresentationCommands" $
        QC.monadicIO . QC.run . testReadPresentationCommands
    ]

-- | A raw input string followed by the expected command.
data ArbitraryCommand = ArbitraryCommand String PresentationCommand
    deriving (Show)

instance QC.Arbitrary ArbitraryCommand where
    arbitrary = QC.elements $
        [ ArbitraryCommand "q"      Exit
        , ArbitraryCommand "\n"     Forward
        , ArbitraryCommand "\DEL"   Backward
        , ArbitraryCommand "h"      Backward
        , ArbitraryCommand "j"      SkipForward
        , ArbitraryCommand "k"      SkipBackward
        , ArbitraryCommand "l"      Forward
        , ArbitraryCommand "\ESC[C" Forward
        , ArbitraryCommand "\ESC[D" Backward
        , ArbitraryCommand "\ESC[B" SkipForward
        , ArbitraryCommand "\ESC[A" SkipBackward
        , ArbitraryCommand "\ESC[6" Forward
        , ArbitraryCommand "\ESC[5" Backward
        , ArbitraryCommand "0"      First
        , ArbitraryCommand "G"      Last
        , ArbitraryCommand "r"      Reload
        ]

testReadPresentationCommands :: [ArbitraryCommand] -> IO Bool
testReadPresentationCommands commands = do
    tmpdir        <- getTemporaryDirectory
    (tmppath, h)  <- IO.openBinaryTempFile tmpdir "patat.input"
    IO.hSetBuffering h IO.NoBuffering
    forM_ commands $ \(ArbitraryCommand s _) -> IO.hPutStr h s
    IO.hSeek h IO.AbsoluteSeek 0
    parsed <- replicateM (length commands) (readPresentationCommand h)
    IO.hClose h
    removeFile tmppath
    return $ [expect | ArbitraryCommand _ expect <- commands] == parsed
