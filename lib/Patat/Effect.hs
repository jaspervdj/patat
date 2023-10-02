--------------------------------------------------------------------------------
module Patat.Effect
    ( EffectId
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
data Effect = Effect
    { eId     :: EffectId
    , eFrames :: NonEmpty Matrix
    , eDelay  :: Int
    }


--------------------------------------------------------------------------------
newEffect :: Size -> PP.Doc -> PP.Doc -> IO Effect
newEffect size frame0 frame1 =
    Effect <$> (EffectId <$> newUnique) <*> pure frames <*> pure 10000
  where
    matrix0 = docToMatrix size frame0
    matrix1 = docToMatrix size frame1
    frames  = slide size matrix0 matrix1


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
        mat <- VM.replicate (rows * cols) $ Elem [] ' '
        for_ [0 .. rows - 1] $ \y -> do
            VM.copy
                (VM.slice (y * cols) (cols - offset) mat)
                (VM.slice (y * cols + offset) (cols - offset) ini)
            VM.copy
                (VM.slice (y * cols + cols - offset) offset mat)
                (VM.slice (y * cols) offset fin)
        pure mat
