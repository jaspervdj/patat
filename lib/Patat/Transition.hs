--------------------------------------------------------------------------------
module Patat.Transition
    ( TransitionId
    , Duration (..)
    , Transition (..)
    , newTransition
    , stepTransition
    ) where


--------------------------------------------------------------------------------
import           Data.Foldable              (for_)
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import           Data.Unique                (Unique, newUnique)
import qualified Data.Vector                as V
import qualified Data.Vector.Mutable        as VM
import           Patat.Presentation.Display (Size (..))
import qualified Patat.PrettyPrint          as PP
import           Patat.PrettyPrint.Matrix


--------------------------------------------------------------------------------
newtype TransitionId = TransitionId Unique deriving (Eq)


--------------------------------------------------------------------------------
newtype Duration = Duration Int deriving (Show)


--------------------------------------------------------------------------------
data Transition = Transition
    { tId     :: TransitionId
    , tSize   :: Size
    , tFrames :: NonEmpty (Matrix, Duration)
    }


--------------------------------------------------------------------------------
newTransition :: Size -> PP.Doc -> PP.Doc -> IO Transition
newTransition termSize frame0 frame1 = do
    unique <- newUnique
    pure $ Transition (TransitionId unique) size frames
  where
    -- The actual part we want to animate does not cover the last row, which is
    -- always empty.
    size    = termSize {sRows = sRows termSize - 1}
    matrix0 = docToMatrix size frame0
    matrix1 = docToMatrix size frame1
    frames  = (\f -> (f, Duration 5000)) <$> slide size matrix0 matrix1


--------------------------------------------------------------------------------
stepTransition :: TransitionId -> Transition -> Maybe Transition
stepTransition tid tr | tid /= tId tr = Just tr
stepTransition _   tr                  = case tFrames tr of
    _ :| []     -> Nothing
    _ :| f : fs -> Just tr {tFrames = f :| fs}


--------------------------------------------------------------------------------
slide :: Size -> Matrix -> Matrix -> NonEmpty Matrix
slide (Size rows cols) initial final =
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
