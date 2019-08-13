--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable        #-}
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
import           Data.Data         (Data)
import           Data.Typeable     (Typeable)
import           Patat.Cleanup


--------------------------------------------------------------------------------
data Config a = Auto | Explicit a deriving (Eq)


--------------------------------------------------------------------------------
data Backend = forall a. A.FromJSON a => Backend (Config a -> IO Handle)


--------------------------------------------------------------------------------
data BackendNotSupported = BackendNotSupported String
    deriving (Data, Show, Typeable)


--------------------------------------------------------------------------------
instance Exception BackendNotSupported


--------------------------------------------------------------------------------
data Handle = Handle
    { hDrawImage  :: FilePath -> IO Cleanup
    }
