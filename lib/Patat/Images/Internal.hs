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
import           Control.Exception  (Exception)
import qualified Data.Aeson         as A
import           Data.Data          (Data)
import qualified Data.List          as L
import           Data.Typeable      (Typeable)
import           Patat.Cleanup
import           System.Environment
import qualified System.IO          as IO

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
    putStr $ if inScreen then "\a\ESC\\" else "\a"
    -- Make sure we don't print a newline after the image, or it may scroll
    -- slightly out of view to the top.  We flush instead.
    IO.hFlush IO.stdout
