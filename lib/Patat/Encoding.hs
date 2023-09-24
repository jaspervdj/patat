-- | When we try to read a file that is encoded in UTF-8, and the system locale
-- is not set to UTF-8, the GHC runtime system will throw an error:
--
-- <https://github.com/jaspervdj/patat/issues/127>
--
-- However, we don't want to force people to use UTF-8 for everything.  So what
-- we do is provide the function 'readFileLenient', which first tries to read
-- the file in the system locale, and then falls back to forcing UTF-8.
--
-- If we forced UTF-8, we also want to propagate that to the output handle;
-- otherwise will get errors when we try to display these characters; so
-- 'propagateEncoding' should be used on the output handle (typically stdout).
module Patat.Encoding
    ( Encoding (..)
    , readFileLenient
    , propagateEncoding
    ) where


--------------------------------------------------------------------------------
import           Control.Exception (throwIO)
import           Control.Monad     (when)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import qualified System.IO         as IO
import qualified System.IO.Error   as IO


--------------------------------------------------------------------------------
data Encoding = SystemEncoding | ForceUtf8Encoding
    deriving (Eq, Show)


--------------------------------------------------------------------------------
readFileLenient :: FilePath -> IO (Encoding, T.Text)
readFileLenient path = IO.catchIOError
    ((,) SystemEncoding <$> T.readFile path)
    (\ioe -> do
        when (IO.isDoesNotExistError ioe) $ throwIO ioe  -- Don't retry on these
        IO.withFile path IO.ReadMode $ \h -> do
            IO.hSetEncoding h IO.utf8_bom
            (,) ForceUtf8Encoding <$> T.hGetContents h)


--------------------------------------------------------------------------------
propagateEncoding :: IO.Handle -> Encoding -> IO ()
propagateEncoding _ SystemEncoding    = pure ()
propagateEncoding h ForceUtf8Encoding = IO.hSetEncoding h IO.utf8
