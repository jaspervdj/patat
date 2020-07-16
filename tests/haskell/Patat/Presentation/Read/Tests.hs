{-# LANGUAGE OverloadedStrings #-}
module Patat.Presentation.Read.Tests
    ( tests
    ) where

import qualified Data.Text               as T
import           Patat.Presentation.Read
import qualified Test.Tasty              as Tasty
import qualified Test.Tasty.HUnit        as Tasty

tests :: Tasty.TestTree
tests = Tasty.testGroup "Patat.Presentation.Read.Tests"
    [ Tasty.testCase "readMetaSettings" $
        case readMetaSettings invalidMetadata of
            Left _  -> pure ()
            Right _ -> Tasty.assertFailure "expecting invalid metadata"
    ]

invalidMetadata :: T.Text
invalidMetadata =
    "---\n\
    \title: mixing tabs and spaces bad\n\
    \author: thoastbrot\n\
    \patat:\n\
    \    images:\n\
    \            backend: 'w3m'\n\
    \            path: '/usr/lib/w3m/w3mimgdisplay'\n\
    \    theme:\n\
    \\theader: [vividBlue,onDullBlack]\n\
    \        emph: [dullBlue,italic]\n\
    \...\n\
    \\n\
    \Hi!"
