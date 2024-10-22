module Patat.Presentation
    ( PresentationSettings (..)
    , defaultPresentationSettings

    , VarGen
    , Var
    , zeroVarGen

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
import           Patat.Presentation.Instruction
import           Patat.Presentation.Interactive
import           Patat.Presentation.Internal
import           Patat.Presentation.Read
