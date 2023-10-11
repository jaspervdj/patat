--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Aeson                  as A
import qualified Data.HashMap.Strict         as HMS
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Patat.Presentation.Settings (TransitionSettings (..))
import           Patat.Transition.Internal
import qualified Patat.Transition.SlideLeft  as SlideLeft


--------------------------------------------------------------------------------
transitions :: HMS.HashMap Text Transition
transitions = HMS.fromList
    [ ("slideLeft", Transition SlideLeft.slideLeft)
    ]


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
