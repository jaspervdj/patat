--------------------------------------------------------------------------------
module Patat.Effect
    ( EffectId
    , Duration (..)
    , Effect (..)
    , newEffect
    , stepEffect
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
newtype EffectId = EffectId Unique deriving (Eq)


--------------------------------------------------------------------------------
newtype Duration = Duration Int deriving (Show)


--------------------------------------------------------------------------------
data Effect = Effect
    { eId     :: EffectId
    , eSize   :: Size
    , eFrames :: NonEmpty (Matrix, Duration)
    }


--------------------------------------------------------------------------------
newEffect :: Size -> PP.Doc -> PP.Doc -> IO Effect
newEffect termSize frame0 frame1 = do
    unique <- newUnique
    pure $ Effect (EffectId unique) size frames
  where
    -- The actual part we want to animate does not cover the last row, which is
    -- always empty.
    size    = termSize {sRows = sRows termSize - 1}
    matrix0 = docToMatrix size frame0
    matrix1 = docToMatrix size frame1
    frames  = (\f -> (f, Duration 10000)) <$> slide size matrix0 matrix1


--------------------------------------------------------------------------------
stepEffect :: EffectId -> Effect -> Maybe Effect
stepEffect eid eff | eid /= eId eff = Just eff
stepEffect _   eff                  = case eFrames eff of
    _ :| []     -> Nothing
    _ :| f : fs -> Just eff {eFrames = f :| fs}


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
