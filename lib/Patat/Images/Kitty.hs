--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Images.Kitty
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Control.Exception     (throwIO)
import           Control.Monad         (unless, void, when)
import qualified Data.Aeson            as A
import           Data.Functor          (($>))
import qualified Data.List             as L
import           Patat.Cleanup         (Cleanup)
import qualified Patat.Images.Internal as Internal
import           System.Environment    (lookupEnv)
import qualified System.IO             as IO
import qualified System.Process        as Process


--------------------------------------------------------------------------------
backend :: Internal.Backend
backend = Internal.Backend new


--------------------------------------------------------------------------------
data Config = Config deriving (Eq)
instance A.FromJSON Config where parseJSON _ = return Config


--------------------------------------------------------------------------------
new :: Internal.Config Config -> IO Internal.Handle
new config = do
    when (config == Internal.Auto) $ do
        term <- lookupEnv "TERM"
        unless (maybe False ("kitty" `L.isInfixOf`) term) $ throwIO $
            Internal.BackendNotSupported "TERM does not indicate kitty"

    return Internal.Handle {Internal.hDrawImage = drawImage}


--------------------------------------------------------------------------------
drawImage :: FilePath -> IO Cleanup
drawImage path = icat ["--align=center", path] $> icat ["--clear"]
  where
    icat args = do
        (Just inh, _, _, ph) <- Process.createProcess (Process.proc "kitty"
            ("+kitten" : "icat" : "--transfer-mode=stream" : "--stdin=no" : args))
            { Process.std_in = Process.CreatePipe
            }
        IO.hClose inh
        void $ Process.waitForProcess ph
