module Patat.Presentation
    ( Presentation (..)
    , readPresentation
    , displayPresentation

    , PresentationCommand
    , readPresentationCommand
    , UpdatedPresentation (..)
    , updatePresentation
    ) where

import           Patat.Presentation.Display
import           Patat.Presentation.Interactive
import           Patat.Presentation.Internal
import           Patat.Presentation.Read
