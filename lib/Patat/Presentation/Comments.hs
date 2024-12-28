--------------------------------------------------------------------------------
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Presentation.Comments
    ( SpeakerNotes (..)
    , speakerNotesToText

    , SpeakerNotesHandle
    , withSpeakerNotesHandle
    , writeSpeakerNotes

    , parseSlideSettings
    ) where


--------------------------------------------------------------------------------
import           Control.Exception           (bracket)
import           Control.Monad               (unless, when)
import qualified Data.IORef                  as IORef
import           Data.List                   (intercalate, intersperse)
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
speakerNotesToText :: SpeakerNotes -> T.Text
speakerNotesToText (SpeakerNotes sn) = T.unlines $ intersperse mempty sn


--------------------------------------------------------------------------------
data SpeakerNotesHandle = SpeakerNotesHandle
    { snhSettings :: !SpeakerNotesSettings
    , snhActive   :: !(IORef.IORef SpeakerNotes)
    }


--------------------------------------------------------------------------------
withSpeakerNotesHandle
    :: SpeakerNotesSettings -> (SpeakerNotesHandle -> IO a) -> IO a
withSpeakerNotesHandle settings = bracket
    (SpeakerNotesHandle settings <$> IORef.newIORef mempty)
    (\_ -> removeFile (snsFile settings))


--------------------------------------------------------------------------------
writeSpeakerNotes
    :: SpeakerNotesHandle -> EncodingFallback -> SpeakerNotes -> IO ()
writeSpeakerNotes h encodingFallback sn = do
    change <- IORef.atomicModifyIORef' (snhActive h) $ \old -> (sn, old /= sn)
    when change $ IO.withFile (snsFile $ snhSettings h) IO.WriteMode $ \ioh ->
        EncodingFallback.withHandle ioh encodingFallback $
        T.hPutStr ioh $ speakerNotesToText sn


--------------------------------------------------------------------------------
data Setting where
    Setting :: String -> (PresentationSettings -> Maybe a) -> Setting


--------------------------------------------------------------------------------
unsupportedSlideSettings :: [Setting]
unsupportedSlideSettings =
    [ Setting "incrementalLists" psIncrementalLists
    , Setting "autoAdvanceDelay" psAutoAdvanceDelay
    , Setting "slideLevel"       psSlideLevel
    , Setting "pandocExtensions" psPandocExtensions
    , Setting "images"           psImages
    , Setting "eval"             psEval
    , Setting "speakerNotes"     psSpeakerNotes
    ]


--------------------------------------------------------------------------------
parseSlideSettings :: PresentationSettings -> Either String PresentationSettings
parseSlideSettings settings = do
    unless (null unsupported) $ Left $
        "the following settings are not supported in slide config blocks: " ++
        intercalate ", " unsupported
    pure settings
  where
    unsupported = do
        setting <- unsupportedSlideSettings
        case setting of
            Setting name f | Just _ <- f settings -> [name]
            Setting _    _                        -> []
