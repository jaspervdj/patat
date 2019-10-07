--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Images.W3m
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Control.Exception      (throwIO)
import           Control.Monad          (unless, void)
import qualified Data.Aeson.TH.Extended as A
import           Data.List              (intercalate)
import           Patat.Cleanup          (Cleanup)
import qualified Patat.Images.Internal  as Internal
import qualified System.Directory       as Directory
import qualified System.Process         as Process
import           Text.Read              (readMaybe)


--------------------------------------------------------------------------------
backend :: Internal.Backend
backend = Internal.Backend new


--------------------------------------------------------------------------------
data Config = Config
    { cPath :: Maybe FilePath
    } deriving (Show)


--------------------------------------------------------------------------------
new :: Internal.Config Config -> IO Internal.Handle
new config = do
    w3m <- findW3m $ case config of
        Internal.Explicit c -> cPath c
        _                   -> Nothing

    return Internal.Handle {Internal.hDrawImage = drawImage w3m}


--------------------------------------------------------------------------------
newtype W3m = W3m FilePath deriving (Show)


--------------------------------------------------------------------------------
findW3m :: Maybe FilePath -> IO W3m
findW3m mbPath
    | Just path <- mbPath = do
        exe <- isExecutable path
        if exe
            then return (W3m path)
            else throwIO $
                    Internal.BackendNotSupported $ path ++ " is not executable"
    | otherwise = W3m <$> find paths
  where
    find []       = throwIO $ Internal.BackendNotSupported
        "w3mimgdisplay executable not found"
    find (p : ps) = do
        exe <- isExecutable p
        if exe then return p else find ps

    paths =
        [ "/usr/lib/w3m/w3mimgdisplay"
        , "/usr/libexec/w3m/w3mimgdisplay"
        , "/usr/lib64/w3m/w3mimgdisplay"
        , "/usr/libexec64/w3m/w3mimgdisplay"
        , "/usr/local/libexec/w3m/w3mimgdisplay"
        ]

    isExecutable path = do
        exists <- Directory.doesFileExist path
        if exists then do
            perms <- Directory.getPermissions path
            return (Directory.executable perms)
        else
            return False


--------------------------------------------------------------------------------
-- | Parses something of the form "<width> <height>\n".
parseWidthHeight :: String -> Maybe (Int, Int)
parseWidthHeight output = case words output of
    [ws, hs] | Just w <- readMaybe ws, Just h <- readMaybe hs ->
        return (w, h)
    _  -> Nothing


--------------------------------------------------------------------------------
getTerminalSize :: W3m -> IO (Int, Int)
getTerminalSize (W3m w3mPath) = do
    output <- Process.readProcess w3mPath ["-test"] ""
    case parseWidthHeight output of
        Just wh -> return wh
        _       -> fail $
            "Patat.Images.W3m.getTerminalSize: " ++
            "Could not parse `w3mimgdisplay -test` output"


--------------------------------------------------------------------------------
getImageSize :: W3m -> FilePath -> IO (Int, Int)
getImageSize (W3m w3mPath) path = do
    output <- Process.readProcess w3mPath [] ("5;" ++ path ++ "\n")
    case parseWidthHeight output of
        Just wh -> return wh
        _       -> fail $
            "Patat.Images.W3m.getImageSize: " ++
            "Could not parse image size using `w3mimgdisplay` for " ++
            path


--------------------------------------------------------------------------------
drawImage :: W3m -> FilePath -> IO Cleanup
drawImage w3m@(W3m w3mPath) path = do
    exists <- Directory.doesFileExist path
    unless exists $ fail $
        "Patat.Images.W3m.drawImage: file does not exist: " ++ path

    tsize <- getTerminalSize w3m
    isize <- getImageSize w3m path
    let (x, y, w, h) = fit tsize isize
        command =
            "0;1;" ++
            show x ++ ";" ++ show y ++ ";" ++ show w ++ ";" ++ show h ++
            ";;;;;" ++ path ++ "\n4;\n3;\n"

    -- Draw image.
    _ <- Process.readProcess w3mPath [] command

    -- Return a 'Cleanup' that clears the image.
    return $ void $ Process.readProcess w3mPath [] $
        "6;" ++ intercalate ";" (map show [x, y, w, h])
  where
    fit :: (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
    fit (tw, th) (iw0, ih0) =
        -- Scale down to width
        let iw1 = if iw0 > tw then tw else iw0
            ih1 = if iw0 > tw then ((ih0 * tw) `div` iw0) else ih0

        -- Scale down to height
            iw2 = if ih1 > th then ((iw1 * th) `div` ih1) else iw1
            ih2 = if ih1 > th then th else ih1

        -- Find position
            x = (tw - iw2) `div` 2
            y = (th - ih2) `div` 2 in

         (x, y, iw2, ih2)


--------------------------------------------------------------------------------
$(A.deriveFromJSON A.dropPrefixOptions ''Config)
