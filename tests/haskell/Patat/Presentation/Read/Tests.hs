{-# LANGUAGE OverloadedStrings #-}
module Patat.Presentation.Read.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Patat.Presentation.Read
import qualified Test.Tasty              as Tasty
import qualified Test.Tasty.HUnit        as Tasty
import qualified Text.Pandoc             as Pandoc


--------------------------------------------------------------------------------
tests :: Tasty.TestTree
tests = Tasty.testGroup "Patat.Presentation.Read.Tests"
    [ testReadMetaSettings
    , testDetectSlideLevel
    ]


--------------------------------------------------------------------------------
testReadMetaSettings :: Tasty.TestTree
testReadMetaSettings = Tasty.testCase "readMetaSettings" $
        case readMetaSettings invalidMetadata of
            Left _  -> pure ()
            Right _ -> Tasty.assertFailure "expecting invalid metadata"
  where
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


--------------------------------------------------------------------------------
testDetectSlideLevel :: Tasty.TestTree
testDetectSlideLevel = Tasty.testGroup "detectSlideLevel"
    [ Tasty.testCase "01" $
        (Tasty.@=?) 1 $ detectSlideLevel $ Pandoc.Pandoc mempty
            [ Pandoc.Header 1 mempty [Pandoc.Str "Intro"]
            , Pandoc.Para [Pandoc.Str "Hi"]
            ]
    , Tasty.testCase "02" $
        (Tasty.@=?) 2 $ detectSlideLevel $ Pandoc.Pandoc mempty
            [ Pandoc.Header 1 mempty [Pandoc.Str "Intro"]
            , Pandoc.Header 2 mempty [Pandoc.Str "Detail"]
            , Pandoc.Para [Pandoc.Str "Hi"]
            ]
    , Tasty.testCase "03" $
        (Tasty.@=?) 2 $ detectSlideLevel $ Pandoc.Pandoc mempty
            [ Pandoc.Header 1 mempty [Pandoc.Str "Intro"]
            , Pandoc.RawBlock "html" "<!-- Some speaker notes -->"
            , Pandoc.Header 2 mempty [Pandoc.Str "Detail"]
            , Pandoc.Para [Pandoc.Str "Hi"]
            ]
    ]
