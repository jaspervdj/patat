--------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
module Patat.Transition.Internal
    ( Duration (..)
    , threadDelayDuration

    , Transition (..)
    , TransitionGen
    , TransitionId
    , TransitionInstance (..)
    , newTransition
    , stepTransition

    , evenlySpacedFrames
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent       (threadDelay)
import qualified Data.Aeson               as A
import           Data.List.NonEmpty       (NonEmpty ((:|)))
import           Data.Maybe               (fromMaybe)
import           Data.Unique              (Unique, newUnique)
import qualified Patat.PrettyPrint        as PP
import           Patat.PrettyPrint.Matrix
import           Patat.Size               (Size (..))
import           System.Random            (StdGen, newStdGen)


--------------------------------------------------------------------------------
newtype Duration = Duration Double  -- Duration in seconds
    deriving (Show)


--------------------------------------------------------------------------------
threadDelayDuration :: Duration -> IO ()
threadDelayDuration (Duration seconds) =
    threadDelay . round $ seconds * 1000 * 1000


--------------------------------------------------------------------------------
data Transition where
    Transition :: A.FromJSON conf => (conf -> TransitionGen) -> Transition


--------------------------------------------------------------------------------
type TransitionGen =
    Size -> Matrix -> Matrix -> StdGen -> NonEmpty (Matrix, Duration)


--------------------------------------------------------------------------------
newtype TransitionId = TransitionId Unique deriving (Eq)


--------------------------------------------------------------------------------
data TransitionInstance = TransitionInstance
    { tiId     :: TransitionId
    , tiSize   :: Size
    , tiFrames :: NonEmpty (Matrix, Duration)
    }


--------------------------------------------------------------------------------
newTransition
    :: TransitionGen -> Size -> PP.Doc -> PP.Doc -> IO TransitionInstance
newTransition tgen termSize frame0 frame1 = do
    unique <- newUnique
    rgen   <- newStdGen
    let frames = tgen size matrix0 matrix1 rgen
    pure $ TransitionInstance (TransitionId unique) size frames
  where
    -- The actual part we want to animate does not cover the last row, which is
    -- always empty.
    size    = termSize {sRows = sRows termSize - 1}
    matrix0 = docToMatrix size frame0
    matrix1 = docToMatrix size frame1


--------------------------------------------------------------------------------
stepTransition :: TransitionId -> TransitionInstance -> Maybe TransitionInstance
stepTransition transId trans | transId /= tiId trans = Just trans
stepTransition _       trans                         = case tiFrames trans of
    _ :| []     -> Nothing
    _ :| f : fs -> Just trans {tiFrames = f :| fs}


--------------------------------------------------------------------------------
-- | Given an optional duration and frame rate, generate a sequence of evenly
-- spaced frames, represented by a number ranging from [0 .. 1].
evenlySpacedFrames
    :: Maybe Double -> Maybe Int -> NonEmpty (Double, Duration)
evenlySpacedFrames mbDuration mbFrameRate =
    frame 0 :| map frame [1 .. frames - 1]
  where
    duration  = fromMaybe 1  mbDuration
    frameRate = fromMaybe 24 mbFrameRate

    frames = round $ duration * fromIntegral frameRate :: Int
    delay  = duration / fromIntegral (frames + 1)

    frame idx = (fromIntegral (idx + 1) / fromIntegral frames, Duration delay)
