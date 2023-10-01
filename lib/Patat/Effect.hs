--------------------------------------------------------------------------------
module Patat.Effect
    ( EffectId
    , Effect (..)
    , newEffect
    , stepEffect
    ) where


--------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Data.Unique        (Unique, newUnique)
import qualified Patat.PrettyPrint  as PP


--------------------------------------------------------------------------------
newtype EffectId = EffectId Unique deriving (Eq)


--------------------------------------------------------------------------------
data Effect = Effect
    { eId     :: EffectId
    , eFrames :: NonEmpty PP.Doc
    , eDelay  :: Int
    }


--------------------------------------------------------------------------------
newEffect :: IO Effect
newEffect =
    Effect <$> (EffectId <$> newUnique) <*> pure frames <*> pure 500000
  where
    frames =
        PP.string "new slide in 3..." <> PP.hardline :|
        PP.string "new slide in 2..." <> PP.hardline :
        PP.string "new slide in 1..." <> PP.hardline :
        []


--------------------------------------------------------------------------------
stepEffect :: EffectId -> Effect -> Maybe Effect
stepEffect eid eff | eid /= eId eff = Just eff
stepEffect _   eff                  = case eFrames eff of
    _ :| []     -> Nothing
    _ :| f : fs -> Just eff {eFrames = f :| fs}
