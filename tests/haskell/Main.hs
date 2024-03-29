module Main where

import qualified Patat.Presentation.Interactive.Tests
import qualified Patat.Presentation.Read.Tests
import qualified Patat.PrettyPrint.Matrix.Tests
import qualified Test.Tasty                           as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "patat"
    [ Patat.Presentation.Interactive.Tests.tests
    , Patat.Presentation.Read.Tests.tests
    , Patat.PrettyPrint.Matrix.Tests.tests
    ]
