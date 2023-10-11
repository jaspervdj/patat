module Patat.Presentation
    ( PresentationSettings (..)
    , defaultPresentationSettings

    , Presentation (..)
    , readPresentation

    , activeSpeakerNotes

    , Size
    , getPresentationSize

    , Display (..)
    , displayPresentation
    , displayPresentationError
    , dumpPresentation

    , PresentationCommand (..)
    , readPresentationCommand
    , UpdatedPresentation (..)
    , updatePresentation
    ) where

import           Patat.Presentation.Display
import           Patat.Presentation.Interactive
import           Patat.Presentation.Internal
import           Patat.Presentation.Read
