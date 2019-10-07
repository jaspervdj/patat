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
    arbitrary = QC.oneof $
        [ return $ ArbitraryCommand "q"      Exit
        , return $ ArbitraryCommand "\n"     Forward
        , return $ ArbitraryCommand "\DEL"   Backward
        , return $ ArbitraryCommand "h"      Backward
        , return $ ArbitraryCommand "j"      SkipForward
        , return $ ArbitraryCommand "k"      SkipBackward
        , return $ ArbitraryCommand "l"      Forward
        , return $ ArbitraryCommand "\ESC[C" Forward
        , return $ ArbitraryCommand "\ESC[D" Backward
        , return $ ArbitraryCommand "\ESC[B" SkipForward
        , return $ ArbitraryCommand "\ESC[A" SkipBackward
        , return $ ArbitraryCommand "\ESC[6" Forward
        , return $ ArbitraryCommand "\ESC[5" Backward
        , return $ ArbitraryCommand "0"      First
        , return $ ArbitraryCommand "G"      Last
        , return $ ArbitraryCommand "r"      Reload
        , do
            n <- QC.choose (1, 1000)
            return $ ArbitraryCommand (show n <> "\n") (Seek n)
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
