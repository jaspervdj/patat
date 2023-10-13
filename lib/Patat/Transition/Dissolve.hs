--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Transition.Dissolve
    ( transition
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended       as A
import qualified Data.Aeson.TH.Extended    as A
import           Data.Bifunctor            (first)
import qualified Data.Vector               as V
import           Patat.PrettyPrint.Matrix
import           Patat.Size                (Size (..))
import           Patat.Transition.Internal
import           System.Random.Stateful


--------------------------------------------------------------------------------
data Config = Config
    { cDuration  :: Maybe (A.FlexibleNum Double)
    , cFrameRate :: Maybe (A.FlexibleNum Int)
    }


--------------------------------------------------------------------------------
transition :: Config -> TransitionGen
transition config (Size rows cols) initial final rgen =
    first frame <$>
    evenlySpacedFrames
        (A.unFlexibleNum <$> cDuration  config)
        (A.unFlexibleNum <$> cFrameRate config)
  where
    -- Generate a random number between 0 and 1 for each position.
    noise :: V.Vector Double
    noise = runStateGen_ rgen $ \g ->
        V.replicateM (rows * cols) (uniformRM (0, 1) g)

    -- Select the initial or final value depending on the noise.
    frame :: Double -> Matrix
    frame t = V.zipWith3
        (\treshold l r -> if t < treshold then l else r)
        noise
        initial
        final


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Config)
