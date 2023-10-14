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
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Traversable            (for)
import           Patat.Presentation.Settings (TransitionSettings (..))
import qualified Patat.Transition.Dissolve   as Dissolve
import           Patat.Transition.Internal
import qualified Patat.Transition.SlideLeft  as SlideLeft
import           System.Random               (uniformR)


--------------------------------------------------------------------------------
data RandomTransitionSettings = RandomTransitionSettings
    { rtsItems :: Maybe (NonEmpty TransitionSettings)
    }


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''RandomTransitionSettings)


--------------------------------------------------------------------------------
random :: NonEmpty TransitionGen -> TransitionGen
random items size matrix0 matrix1 rg0 =
    let (idx, rg1) = uniformR (0, length items - 1) rg0 in
    (items NonEmpty.!! idx) size matrix0 matrix1 rg1


--------------------------------------------------------------------------------
transitions :: NonEmpty (Text, Transition)
transitions =
    ("dissolve",  Transition Dissolve.transition) :|
    ("slideLeft", Transition SlideLeft.transition) : []


--------------------------------------------------------------------------------
transitionTable :: HMS.HashMap Text Transition
transitionTable = foldMap (uncurry HMS.singleton) transitions


--------------------------------------------------------------------------------
parseTransitionSettings
    :: TransitionSettings -> Either String TransitionGen
parseTransitionSettings ts
    -- Random is treated specially here.
    | ty == "random" = fmap random $ do
        settings <- A.resultToEither . A.fromJSON . A.Object $ tsParams ts
        case rtsItems settings of
            -- Items specified: parse those
            Just items -> traverse parseTransitionSettings items
            -- No items specified: parse default transition settings.
            Nothing -> for transitions $ \(typ, _) -> parseTransitionSettings
                TransitionSettings {tsType = typ, tsParams = mempty}
    -- Found the transition type.
    | Just (Transition f) <- HMS.lookup ty transitionTable =
        fmap (f $) . first (\err ->
            "could not parse " ++ T.unpack ty ++ " transition: " ++ err) .
        A.resultToEither . A.fromJSON . A.Object $ tsParams ts
    -- Not found, error.
    | otherwise = Left $ "unknown transition type: " ++ show ty
  where
    ty = tsType ts
