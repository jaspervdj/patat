--------------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
module Patat.Images.Internal
    ( Config (..)
    , Backend (..)
    , BackendNotSupported (..)
    , Handle (..)
    , withEscapeSequence
    ) where


--------------------------------------------------------------------------------
import           Control.Exception    (Exception)
import qualified Data.Aeson           as A
import           Data.Data            (Data)
import           Data.Typeable        (Typeable)
import           Patat.Cleanup
import           System.Environment
import qualified Data.List            as L

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

--------------------------------------------------------------------------------
withEscapeSequence :: IO () -> IO ()
withEscapeSequence f = do
    term <- lookupEnv "TERM"
    let inScreen = maybe False ("screen" `L.isPrefixOf`) term
    putStr $ if inScreen then "\ESCPtmux;\ESC\ESC]" else "\ESC]"
    f
    putStrLn $ if inScreen then "\a\ESC\\" else "\a"
