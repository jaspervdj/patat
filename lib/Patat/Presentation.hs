module Patat.Presentation
    ( PresentationSettings (..)
    , defaultPresentationSettings

    , UniqueGen
    , Unique
    , zeroUniqueGen

    , Presentation (..)
    , readPresentation

    , activeSpeakerNotes
    , activeVars

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
import           Patat.Unique
