--------------------------------------------------------------------------------
module Patat.Effect
    ( Effect (..)
    ) where


--------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty)
import           Data.Unique        (Unique)
import qualified Patat.PrettyPrint  as PP


--------------------------------------------------------------------------------
data Effect = Effect
    { eId     :: Unique
    , eFrames :: NonEmpty PP.Doc
    , eDelay  :: Int
    }
