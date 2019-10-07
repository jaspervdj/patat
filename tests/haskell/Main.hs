module Main where

import qualified Patat.Presentation.Interactive.Tests
import qualified Test.Tasty                           as Tasty

main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "patat"
    [ Patat.Presentation.Interactive.Tests.tests
    ]
