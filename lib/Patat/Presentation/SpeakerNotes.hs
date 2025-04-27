--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Patat.Presentation.SpeakerNotes
    ( SpeakerNotes (..)
    , toText

    , Handle
    , withHandle
    , write

    , parseSlideSettings
    ) where


--------------------------------------------------------------------------------
import           Control.Exception           (bracket)
import           Control.Monad               (when)
import qualified Data.IORef                  as IORef
import           Data.List                   (intersperse)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Patat.EncodingFallback      (EncodingFallback)
import qualified Patat.EncodingFallback      as EncodingFallback
import           Patat.Presentation.Settings
import           System.Directory            (removeFile)
import qualified System.IO                   as IO


--------------------------------------------------------------------------------
newtype SpeakerNotes = SpeakerNotes [T.Text]
    deriving (Eq, Monoid, Semigroup, Show)


--------------------------------------------------------------------------------
toText :: SpeakerNotes -> T.Text
toText (SpeakerNotes sn) = T.unlines $ intersperse mempty sn


--------------------------------------------------------------------------------
data Handle = Handle
    { hSettings :: !SpeakerNotesSettings
    , hActive   :: !(IORef.IORef SpeakerNotes)
    }


--------------------------------------------------------------------------------
withHandle
    :: SpeakerNotesSettings -> (Handle -> IO a) -> IO a
withHandle settings = bracket
    (Handle settings <$> IORef.newIORef mempty)
    (\_ -> removeFile (snsFile settings))


--------------------------------------------------------------------------------
write
    :: Handle -> EncodingFallback -> SpeakerNotes -> IO ()
write h encodingFallback sn = do
    change <- IORef.atomicModifyIORef' (hActive h) $ \old -> (sn, old /= sn)
    when change $ IO.withFile (snsFile $ hSettings h) IO.WriteMode $ \ioh ->
        EncodingFallback.withHandle ioh encodingFallback $
        T.hPutStr ioh $ toText sn
