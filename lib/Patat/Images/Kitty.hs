--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Images.Kitty
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Control.Exception           (throwIO)
import           Control.Monad               (unless, void, when)
import qualified Data.Aeson                  as A
import qualified Data.List                   as L
import           Patat.Cleanup               (Cleanup)
import qualified Patat.Images.Internal       as Internal
import Data.Functor (($>))
import           System.Environment          (lookupEnv)
import           System.Process              (readProcess)


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
    icat args = void $ readProcess
        "kitty" ("+kitten" : "icat" : "--transfer-mode=stream" : args) ""
