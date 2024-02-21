--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Transition.Matrix
    ( transition
    ) where


--------------------------------------------------------------------------------
import           Control.Monad             (forM_, guard, when)
import qualified Data.Aeson.Extended       as A
import qualified Data.Aeson.TH.Extended    as A
import           Data.Bifunctor            (first)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
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
data Particle = Particle
    { pX        :: Double
    , pInitialY :: Double
    , pFinalY   :: Double
    , pSpeed    :: Double
    , pCell     :: Cell
    }


--------------------------------------------------------------------------------
particleY :: Particle -> Double -> Double
particleY p t = pInitialY p * (1 - t') + pFinalY p * t'
  where
    t' = min 1 (pSpeed p * t)


--------------------------------------------------------------------------------
transition :: Config -> TransitionGen
transition config (Size rows cols) initial final rgen =
    first frame <$>
    evenlySpacedFrames
        (A.unFlexibleNum <$> cDuration  config)
        (A.unFlexibleNum <$> cFrameRate config)
  where
    speeds :: V.Vector Double
    speeds = runStateGen_ rgen $ \g ->
        V.replicateM (rows * cols) (uniformRM (1, 3) g)

    up :: V.Vector Bool
    up = runStateGen_ rgen $ \g ->
        V.replicateM (rows * cols) (uniformM g)

    initialParticles :: [Particle]
    initialParticles = do
        (x, y, cell) <- posCells initial
        let idx = y * cols + x
        speed <- [speeds V.! idx, speeds V.! idx * 1.5, speeds V.! idx * 2]
        pure Particle
            { pX        = fromIntegral x
            , pInitialY = fromIntegral y
            , pFinalY   = if up V.! idx then 0 else fromIntegral rows
            , pSpeed    = speed
            , pCell     = cell
            }

    finalParticles :: [Particle]
    finalParticles = do
        (x, y, cell) <- posCells final
        let idx = y * cols + x
        speed <- [speeds V.! idx, speeds V.! idx * 1.5, speeds V.! idx * 2]
        pure Particle
            { pX        = fromIntegral x
            , pInitialY = if up V.! idx then -1 else fromIntegral rows
            , pFinalY   = fromIntegral y
            , pSpeed    = speed
            , pCell     = cell
            }

    posCells :: Matrix -> [(Int, Int, Cell)]
    posCells mat = do
        y <- [0 .. rows - 1]
        x <- [0 .. cols - 1]
        let cell = mat V.! (y * cols + x)
        guard . not $ cell == emptyCell
        pure (x, y, cell)

    -- Select the initial or final value depending on the noise.
    frame :: Double -> Matrix
    frame t = V.create $ do
        mat <- VM.replicate (rows * cols) emptyCell
        forM_ (initialParticles ++ finalParticles) $ \particle ->
            let y = round $ particleY particle t
                x = round $ pX particle
                idx = y * cols + x in
            when (x >= 0 && x < cols && y >= 0 && y < rows) $
                VM.write mat idx $ pCell particle
        pure mat


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Config)
