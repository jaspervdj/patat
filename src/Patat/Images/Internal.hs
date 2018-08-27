--------------------------------------------------------------------------------
{-# LANGUAGE ExistentialQuantification #-}
module Patat.Images.Internal
    ( Config (..)
    , Backend (..)
    , BackendNotSupported (..)
    , Handle (..)
    ) where


--------------------------------------------------------------------------------
import           Control.Exception (Exception)
import qualified Data.Aeson        as A


--------------------------------------------------------------------------------
data Config a = Auto | Explicit a


--------------------------------------------------------------------------------
data Backend = forall a. A.FromJSON a => Backend (Config a -> IO Handle)


--------------------------------------------------------------------------------
data BackendNotSupported = BackendNotSupported String
    deriving (Show)


--------------------------------------------------------------------------------
instance Exception BackendNotSupported


--------------------------------------------------------------------------------
data Handle = Handle
    { hDrawImage  :: FilePath -> IO ()
    }
