--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedLists #-}
module Patat.PrettyPrint.Matrix.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Patat.PrettyPrint
import           Patat.PrettyPrint.Matrix
import           Patat.Size
import qualified System.Console.ANSI      as Ansi
import qualified Test.Tasty               as Tasty
import qualified Test.Tasty.HUnit         as Tasty
import           Test.Tasty.HUnit         ((@?=))


--------------------------------------------------------------------------------
tests :: Tasty.TestTree
tests = Tasty.testGroup "Patat.PrettyPrint.Matrix.Tests"
    [ testDocToMatrix
    ]


--------------------------------------------------------------------------------
testDocToMatrix :: Tasty.TestTree
testDocToMatrix = Tasty.testGroup "docToMatrix"
    [ Tasty.testCase "wcwidth" $
        docToMatrix
            (Size 2 10)
            (string "wcwidth:" <$$> ansi [green] (string "コンニチハ")) @?=
        [ c 'w', c 'c', c 'w', c 'i', c 'd', c 't', c 'h', c ':', e, e
        , cg 'コ', e, cg 'ン', e, cg 'ニ', e, cg 'チ', e, cg 'ハ', e
        ]
    , Tasty.testCase "wrap" $
        docToMatrix
            (Size 4 4)
            (string "fits" <$$> string "wrapped" <$$> string "fits") @?=
        [ c 'f', c 'i', c 't', c 's'
        , c 'w', c 'r', c 'a', c 'p'
        , c 'p', c 'e', c 'd', e
        , c 'f', c 'i', c 't', c 's'
        ]
    , Tasty.testCase "overflow" $
        docToMatrix
            (Size 3 3)
            (string "overflowed") @?=
        [ c 'o', c 'v', c 'e'
        , c 'r', c 'f', c 'l'
        , c 'o', c 'w', c 'e'
        ]
    ]
  where
    green = Ansi.SetColor Ansi.Foreground Ansi.Vivid Ansi.Green

    c  = Cell []
    cg = Cell [green]
    e  = emptyCell
