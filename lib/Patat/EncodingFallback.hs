-- | When we try to read a file that is encoded in UTF-8, and the system locale
-- is not set to UTF-8, the GHC runtime system will throw an error:
--
-- <https://github.com/jaspervdj/patat/issues/127>
--
-- However, we don't want to force people to use UTF-8 for everything.  So what
-- we do is provide a replacement readFile, which first tries to read the file
-- in the system locale, and then falls back to forcing UTF-8.
--
-- If we forced UTF-8, we also want to propagate that to the output handle;
-- otherwise will get errors when we try to display these characters; so
-- 'propagateEncoding' should be used on the output handle (typically stdout).
module Patat.EncodingFallback
    ( EncodingFallback (..)
    , readFile
    , withHandle
    ) where


--------------------------------------------------------------------------------
import           Control.Exception (bracket, throwIO)
import           Control.Monad     (when)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import           Prelude           hiding (readFile)
import qualified System.IO         as IO
import qualified System.IO.Error   as IO


--------------------------------------------------------------------------------
data EncodingFallback = NoFallback | Utf8Fallback
    deriving (Eq, Show)


--------------------------------------------------------------------------------
readFile :: FilePath -> IO (EncodingFallback, T.Text)
readFile path = IO.catchIOError readSystem $ \ioe -> do
    when (IO.isDoesNotExistError ioe) $ throwIO ioe  -- Don't retry on these
    readUtf8
  where
    readSystem = ((,) NoFallback <$> T.readFile path)
    readUtf8   = IO.withFile path IO.ReadMode $ \h -> do
        IO.hSetEncoding h IO.utf8_bom
        (,) Utf8Fallback <$> T.hGetContents h


--------------------------------------------------------------------------------
withHandle :: IO.Handle -> EncodingFallback -> IO a -> IO a
withHandle _ NoFallback   mx = mx
withHandle h Utf8Fallback mx = bracket
    (do
        mbOld <- IO.hGetEncoding h
        IO.hSetEncoding h IO.utf8
        pure mbOld)
    (\mbOld -> traverse (IO.hSetEncoding h) mbOld)
    (\_ -> mx)
