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
    , new
    , write
    ) where


--------------------------------------------------------------------------------
import           Control.Monad          (when)
import qualified Data.Aeson.TH.Extended as A
import qualified Data.IORef             as IORef
import           Data.List              (intersperse)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
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
new :: Settings -> IO Handle
new settings = Handle settings <$> IORef.newIORef mempty


--------------------------------------------------------------------------------
write :: Handle -> SpeakerNotes -> IO ()
write h sn = do
    change <- IORef.atomicModifyIORef' (hActive h) $ \old -> (sn, old /= sn)
    when change $ T.writeFile (sFile $ hSettings h) $ toText sn


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Settings)
