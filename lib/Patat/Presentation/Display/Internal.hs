--------------------------------------------------------------------------------
module Patat.Presentation.Display.Internal
    ( DisplaySettings (..)
    , themed
    ) where


--------------------------------------------------------------------------------
import           Patat.Size        (Size)
import qualified Patat.PrettyPrint as PP
import qualified Patat.Theme       as Theme
import qualified Skylighting       as Skylighting


--------------------------------------------------------------------------------
data DisplaySettings = DisplaySettings
    { dsSize      :: !Size
    , dsTheme     :: !Theme.Theme
    , dsSyntaxMap :: !Skylighting.SyntaxMap
    }


--------------------------------------------------------------------------------
themed
    :: DisplaySettings -> (Theme.Theme -> Maybe Theme.Style) -> PP.Doc -> PP.Doc
themed ds f = case f (dsTheme ds) of
    Nothing                  -> id
    Just (Theme.Style [])    -> id
    Just (Theme.Style codes) -> PP.ansi codes
