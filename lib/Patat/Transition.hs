--------------------------------------------------------------------------------
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Patat.Transition
    ( Duration (..)
    , threadDelayDuration
    , TransitionGen
    , TransitionId
    , TransitionInstance (..)
    , parseTransitionSettings
    , newTransition
    , stepTransition
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended         as A
import qualified Data.Aeson.TH.Extended      as A
import           Data.Bifunctor              (first)
import qualified Data.HashMap.Strict         as HMS
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Patat.Presentation.Settings (TransitionSettings (..))
import qualified Patat.Transition.Dissolve   as Dissolve
import           Patat.Transition.Internal
import qualified Patat.Transition.SlideLeft  as SlideLeft
import           System.Random               (uniformR)


--------------------------------------------------------------------------------
data Random a = Random
    { rItems :: NonEmpty a
    } deriving (Foldable, Functor, Traversable)


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Random)


--------------------------------------------------------------------------------
random :: Random TransitionGen -> TransitionGen
random r size matrix0 matrix1 rg0 =
    let (idx, rg1) = uniformR (0, length (rItems r) - 1) rg0 in
    (rItems r NonEmpty.!! idx) size matrix0 matrix1 rg1


--------------------------------------------------------------------------------
transitions :: HMS.HashMap Text Transition
transitions = HMS.fromList
    [ ("dissolve",  Transition Dissolve.transition)
    , ("slideLeft", Transition SlideLeft.transition)
    ]


--------------------------------------------------------------------------------
parseTransitionSettings
    :: TransitionSettings -> Either String TransitionGen
parseTransitionSettings ts = case HMS.lookup ty transitions of
    _ | ty == "random"  -> do
        settings <- A.resultToEither . A.fromJSON . A.Object $ tsParams ts
        tgens <- traverse parseTransitionSettings settings
        pure $ random tgens
    Nothing             -> Left $ "unknown transition type: " ++ show ty
    Just (Transition f) ->
        fmap (f $) .
        first (\err ->
            "could not parse " ++ T.unpack ty ++ " transition: " ++ err) .
        A.resultToEither . A.fromJSON . A.Object $ tsParams ts
  where
   ty = tsType ts
