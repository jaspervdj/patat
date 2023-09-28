--------------------------------------------------------------------------------
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Presentation.Comments
    ( Comment (..)
    , parse
    , remove
    , split
    , partition

    , SpeakerNotes
    , speakerNotesToText

    , SpeakerNotesHandle
    , withSpeakerNotesHandle
    , writeSpeakerNotes

    , parseSlideSettings
    ) where


--------------------------------------------------------------------------------
import           Control.Applicative         ((<|>))
import           Control.Exception           (bracket)
import           Control.Monad               (unless, when)
import           Data.Function               (on)
import qualified Data.IORef                  as IORef
import           Data.List                   (intercalate, intersperse)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as T
import qualified Data.Yaml                   as Yaml
import           Patat.EncodingFallback      (EncodingFallback)
import qualified Patat.EncodingFallback      as EncodingFallback
import           Patat.Presentation.Settings
import           System.Directory            (removeFile)
import qualified System.IO                   as IO
import qualified Text.Pandoc                 as Pandoc


--------------------------------------------------------------------------------
data Comment = Comment
    { cSpeakerNotes :: SpeakerNotes
    , cConfig       :: Either String PresentationSettings
    } deriving (Show)


--------------------------------------------------------------------------------
instance Semigroup Comment where
    l <> r = Comment
        { cSpeakerNotes = on (<>) cSpeakerNotes l r
        , cConfig       = case (cConfig l, cConfig r) of
            (Left err, _       ) -> Left err
            (Right _,  Left err) -> Left err
            (Right x,  Right y ) -> Right (x <> y)
        }


--------------------------------------------------------------------------------
instance Monoid Comment where
    mappend = (<>)
    mempty  = Comment mempty (Right mempty)


--------------------------------------------------------------------------------
parse :: Pandoc.Block -> Maybe Comment
parse (Pandoc.RawBlock "html" t0) =
    (do
        t1 <- T.stripPrefix "<!--config:" t0
        t2 <- T.stripSuffix "-->" t1
        pure . Comment mempty $ case Yaml.decodeEither' (T.encodeUtf8 t2) of
            Left err  -> Left (show err)
            Right obj -> Right obj) <|>
    (do
        t1 <- T.stripPrefix "<!--" t0
        t2 <- T.stripSuffix "-->" t1
        pure $ Comment (SpeakerNotes [T.strip t2]) (Right mempty))
parse _ = Nothing


--------------------------------------------------------------------------------
remove :: [Pandoc.Block] -> [Pandoc.Block]
remove = snd . partition


--------------------------------------------------------------------------------
-- | Take all comments from the front of the list.  Return those and the
-- remaining blocks.
split :: [Pandoc.Block] -> (Comment, [Pandoc.Block])
split = go []
  where
    go sn []                           = (mconcat (reverse sn), [])
    go sn (x : xs) | Just s <- parse x = go (s : sn) xs
    go sn xs                           = (mconcat (reverse sn), xs)


--------------------------------------------------------------------------------
-- | Partition the list into speaker notes and other blocks.
partition :: [Pandoc.Block] -> (Comment, [Pandoc.Block])
partition = go [] []
  where
    go sn bs []                           = (mconcat (reverse sn), reverse bs)
    go sn bs (x : xs) | Just s <- parse x = go (s : sn) bs xs
    go sn bs (x : xs)                     = go sn (x : bs) xs


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
parseSlideSettings :: Comment -> Either String PresentationSettings
parseSlideSettings c = do
    settings <- cConfig c
    let unsupported = do
            setting <- unsupportedSlideSettings
            case setting of
                Setting name f | Just _ <- f settings -> [name]
                Setting _    _                        -> []
    unless (null unsupported) $ Left $
        "the following settings are not supported in slide config blocks: " ++
        intercalate ", " unsupported
    pure settings
