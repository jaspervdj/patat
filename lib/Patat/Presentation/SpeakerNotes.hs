--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Presentation.SpeakerNotes
    ( SpeakerNotes
    , parse
    , toText
    , remove
    , split
    , partition

    , Settings
    , Handle
    , with
    , write
    ) where


--------------------------------------------------------------------------------
import           Control.Exception      (bracket)
import           Control.Monad          (when)
import qualified Data.Aeson.TH.Extended as A
import qualified Data.IORef             as IORef
import           Data.List              (intersperse)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Patat.EncodingFallback (EncodingFallback)
import qualified Patat.EncodingFallback as EncodingFallback
import           System.Directory       (removeFile)
import qualified System.IO              as IO
import qualified Text.Pandoc            as Pandoc


--------------------------------------------------------------------------------
newtype SpeakerNotes = SpeakerNotes [T.Text]
    deriving (Eq, Monoid, Semigroup, Show)


--------------------------------------------------------------------------------
parse :: Pandoc.Block -> Maybe SpeakerNotes
parse (Pandoc.RawBlock "html" t0) = do
    t1 <- T.stripPrefix "<!--" t0
    t2 <- T.stripSuffix "-->" t1
    pure $ SpeakerNotes [T.strip t2]
parse _ = Nothing


--------------------------------------------------------------------------------
toText :: SpeakerNotes -> T.Text
toText (SpeakerNotes sn) = T.unlines $ intersperse mempty sn


--------------------------------------------------------------------------------
remove :: [Pandoc.Block] -> [Pandoc.Block]
remove = snd . partition


--------------------------------------------------------------------------------
-- | Take all speaker notes from the front of the list.  Return those and the
-- remaining blocks.
split :: [Pandoc.Block] -> (SpeakerNotes, [Pandoc.Block])
split = go []
  where
    go sn []                           = (mconcat (reverse sn), [])
    go sn (x : xs) | Just s <- parse x = go (s : sn) xs
    go sn xs                           = (mconcat (reverse sn), xs)


--------------------------------------------------------------------------------
-- | Partition the list into speaker notes and other blocks.
partition :: [Pandoc.Block] -> (SpeakerNotes, [Pandoc.Block])
partition = go [] []
  where
    go sn bs []                           = (mconcat (reverse sn), reverse bs)
    go sn bs (x : xs) | Just s <- parse x = go (s : sn) bs xs
    go sn bs (x : xs)                     = go sn (x : bs) xs


--------------------------------------------------------------------------------
data Settings = Settings
    { sFile :: !FilePath
    } deriving (Show)


--------------------------------------------------------------------------------
data Handle = Handle
    { hSettings :: !Settings
    , hActive   :: !(IORef.IORef SpeakerNotes)
    }


--------------------------------------------------------------------------------
with :: Settings -> (Handle -> IO a) -> IO a
with settings = bracket
    (Handle settings <$> IORef.newIORef mempty)
    (\_ -> removeFile (sFile settings))


--------------------------------------------------------------------------------
write :: Handle -> EncodingFallback -> SpeakerNotes -> IO ()
write h encodingFallback sn = do
    change <- IORef.atomicModifyIORef' (hActive h) $ \old -> (sn, old /= sn)
    when change $ IO.withFile (sFile $ hSettings h) IO.WriteMode $ \ioh ->
        EncodingFallback.withHandle ioh encodingFallback $
        T.hPutStr ioh $ toText sn


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Settings)
