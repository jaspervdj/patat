module Patat.Presentation
    ( PresentationSettings (..)
    , defaultPresentationSettings

    , Presentation (..)
    , readPresentation

    , Size
    , getDisplaySize

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
