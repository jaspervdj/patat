--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Transition.SlideLeft
    ( transition
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended       as A
import qualified Data.Aeson.TH.Extended    as A
import           Data.Bifunctor            (first)
import           Data.Foldable             (for_)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import           Patat.PrettyPrint.Matrix
import           Patat.Size                (Size (..))
import           Patat.Transition.Internal


--------------------------------------------------------------------------------
data Config = Config
    { cDuration  :: Maybe (A.FlexibleNum Double)
    , cFrameRate :: Maybe (A.FlexibleNum Int)
    }


--------------------------------------------------------------------------------
transition :: Config -> TransitionGen
transition config (Size rows cols) initial final _rgen =
    first frame <$>
    evenlySpacedFrames
        (A.unFlexibleNum <$> cDuration  config)
        (A.unFlexibleNum <$> cFrameRate config)
  where
    frame :: Double -> Matrix
    frame t = V.create $ do
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
      where
        offset = max 0 . min cols . (round :: Double -> Int) $
            t * fromIntegral cols


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Config)
