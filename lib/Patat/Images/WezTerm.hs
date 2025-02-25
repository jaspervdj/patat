--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Patat.Images.WezTerm
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Codec.Picture           (DynamicImage,
                                          Image (imageHeight, imageWidth),
                                          decodeImage, dynamicMap)
import           Control.Exception       (throwIO)
import           Control.Monad           (unless, when)
import qualified Data.Aeson              as A
import qualified Data.ByteString         as B
import qualified Data.ByteString.Base64  as B64
import qualified Data.Text.Lazy          as TL
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Patat.Cleanup           (Cleanup)
import qualified Patat.Images.Internal   as Internal
import           System.Directory        (findExecutable)
import           System.Environment      (lookupEnv)
import           System.Process          (readProcess)


--------------------------------------------------------------------------------
backend :: Internal.Backend
backend = Internal.Backend new


--------------------------------------------------------------------------------
data Config = Config deriving (Eq)
instance A.FromJSON Config where parseJSON _ = return Config


--------------------------------------------------------------------------------
data Pane =
    Pane { paneSize     :: Size
         , paneIsActive :: Bool
         } deriving (Show)

instance A.FromJSON Pane where
    parseJSON = A.withObject "Pane" $ \o -> Pane
        <$> o A..: "size"
        <*> o A..: "is_active"


--------------------------------------------------------------------------------
data Size =
    Size { sizePixelWidth  :: Int
         , sizePixelHeight :: Int
         } deriving (Show)

instance A.FromJSON Size where
    parseJSON = A.withObject "Size" $ \o -> Size
        <$> o A..: "pixel_width"
        <*> o A..: "pixel_height"


--------------------------------------------------------------------------------
new :: Internal.Config Config -> IO Internal.Handle
new config = do
    when (config == Internal.Auto) $ do
        termProgram <- lookupEnv "TERM_PROGRAM"
        unless (termProgram == Just "WezTerm") $ throwIO $
            Internal.BackendNotSupported "TERM_PROGRAM not WezTerm"

    return Internal.Handle {Internal.hDrawImage = drawImage}


--------------------------------------------------------------------------------
drawImage :: FilePath -> IO Cleanup
drawImage path = do
    content <- B.readFile path

    wez <- wezExecutable
    resp <- fmap (encodeUtf8 . TL.pack) $ readProcess wez ["cli", "list", "--format", "json"] []
    let panes = (A.decode resp :: Maybe [Pane])

    Internal.withEscapeSequence $ do
        putStr "1337;File=inline=1;doNotMoveCursor=1;"
        case decodeImage content of
            Left _    -> pure ()
            Right img -> putStr $ wezArString (imageAspectRatio img) (activePaneAspectRatio panes)
        putStr ":"
        B.putStr (B64.encode content)
    return mempty


--------------------------------------------------------------------------------
wezArString  :: Double -> Double -> String
wezArString i p | i < p     = "width=auto;height=95%;"
                | otherwise = "width=100%;height=auto;"


--------------------------------------------------------------------------------
wezExecutable :: IO String
wezExecutable = do
    w <- findExecutable "wezterm.exe"
    case w of
        Nothing -> return "wezterm"
        Just x  -> return x


--------------------------------------------------------------------------------
imageAspectRatio  :: DynamicImage -> Double
imageAspectRatio i = imgW i / imgH i
    where
        imgH = fromIntegral . (dynamicMap imageHeight)
        imgW = fromIntegral . (dynamicMap imageWidth)


--------------------------------------------------------------------------------
paneAspectRatio :: Pane -> Double
paneAspectRatio p = paneW p / paneH p
    where
        paneH = fromIntegral . sizePixelHeight . paneSize
        paneW = fromIntegral . sizePixelWidth . paneSize


--------------------------------------------------------------------------------
activePaneAspectRatio :: Maybe [Pane] -> Double
activePaneAspectRatio Nothing = defaultAr -- This should never happen
activePaneAspectRatio (Just x) =
    case filter paneIsActive x of
        [p] -> paneAspectRatio p
        _   -> defaultAr                  -- This shouldn't either


--------------------------------------------------------------------------------
defaultAr :: Double
defaultAr = (4 / 3 :: Double)             -- Good enough for a VT100


