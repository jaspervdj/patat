--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Transition.SlideLeft
    ( slideLeft
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended       as A
import qualified Data.Aeson.TH.Extended    as A
import           Data.Foldable             (for_)
import           Data.List.NonEmpty        (NonEmpty ((:|)))
import           Data.Maybe                (fromMaybe)
import qualified Data.Vector               as V
import qualified Data.Vector.Mutable       as VM
import           Patat.PrettyPrint.Matrix
import           Patat.Size                (Size (..))
import           Patat.Transition.Internal


--------------------------------------------------------------------------------
data Config = Config
    { cDuration :: Maybe (A.FlexibleNum Double)
    , cFrames   :: Maybe (A.FlexibleNum Int)
    }


--------------------------------------------------------------------------------
slideLeft :: Config -> TransitionGen
slideLeft config (Size rows cols) initial final _rgen =
    fmap (\f -> (f, Duration delay)) $
    frame 0 :| map frame [1 .. frames - 1]
  where
    duration = fromMaybe 1  $ A.unFlexibleNum <$> cDuration config
    frames   = fromMaybe 24 $ A.unFlexibleNum <$> cFrames   config

    delay = duration / fromIntegral (frames + 1)

    frame :: Int -> Matrix
    frame idx = V.create $ do
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
            fromIntegral (idx + 1) / fromIntegral frames * fromIntegral cols


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Config)
