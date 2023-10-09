--------------------------------------------------------------------------------
module Patat.Transition.SlideLeft
    ( slideLeft
    ) where


--------------------------------------------------------------------------------
import           Data.Foldable             (for_)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import           Patat.PrettyPrint.Matrix
import           Patat.Size                (Size (..))
import           Patat.Transition.Internal


--------------------------------------------------------------------------------
slideLeft :: () -> TransitionGen
slideLeft _noconf (Size rows cols) initial final _rgen =
    fmap (\f -> (f, Duration 5000)) $
    initial :| map frame [1 .. cols - 1] ++ [final]
  where
    frame offset = V.create $ do
        ini <- V.unsafeThaw initial
        fin <- V.unsafeThaw final
        mat <- VM.replicate (rows * cols) emptyCell
        for_ [0 .. rows - 1] $ \y -> do
            VM.copy
                (VM.slice (y * cols) (cols - offset) mat)
                (VM.slice (y * cols + offset) (cols - offset) ini)
            VM.copy
                (VM.slice (y * cols + cols - offset) offset mat)
                (VM.slice (y * cols) offset fin)
        pure mat
