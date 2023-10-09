--------------------------------------------------------------------------------
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Patat.Transition
    ( TransitionGen
    , Duration (..)
    , TransitionId
    , TransitionInstance (..)
    , parseTransitionSettings
    , newTransition
    , stepTransition
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson                  as A
import           Data.Foldable               (for_)
import qualified Data.HashMap.Strict         as HMS
import           Data.List.NonEmpty          (NonEmpty ((:|)))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Unique                 (Unique, newUnique)
import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import           Patat.Presentation.Settings (TransitionSettings (..))
import qualified Patat.PrettyPrint           as PP
import           Patat.PrettyPrint.Matrix
import           Patat.Size                  (Size (..))
import           System.Random               (StdGen, newStdGen)


--------------------------------------------------------------------------------
newtype Duration = Duration Int deriving (Show)


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
parseTransitionSettings
    :: TransitionSettings -> Either String TransitionGen
parseTransitionSettings ts = case HMS.lookup ty transitions of
    Nothing             -> Left $ "unknown transition type: " ++ show ty
    Just (Transition f) -> case A.fromJSON (A.Object $ tsParams ts) of
        A.Success conf -> Right $ f conf
        A.Error   err  -> Left $
            "could not parse " ++ T.unpack ty ++ " transition: " ++ err
  where
   ty = tsType ts


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
slide :: () -> Size -> Matrix -> Matrix -> gen -> NonEmpty (Matrix, Duration)
slide _noconf (Size rows cols) initial final _rgen =
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


--------------------------------------------------------------------------------
transitions :: HMS.HashMap Text Transition
transitions = HMS.fromList
    [ ("slideLeft", Transition slide)
    ]
