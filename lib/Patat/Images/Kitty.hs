--------------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Patat.Images.Kitty
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Control.Exception     (throwIO)
import           Control.Monad         (unless, void, when)
import qualified Data.Aeson            as A
import qualified Data.List             as L
import           Patat.Cleanup         (Cleanup)
import qualified Patat.Images.Internal as Internal
import           Patat.Size
import           System.Environment    (lookupEnv)
import qualified System.IO             as IO
import qualified System.Process        as Process


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
data Place = Place Int Int Int Int  -- w, h, x, y


--------------------------------------------------------------------------------
instance Show Place where
    show (Place w h x y) =
        show w ++ "x" ++ show h ++ "@" ++ show x ++ "x" ++ show y


--------------------------------------------------------------------------------
drawImage :: Internal.DrawImageOptions -> FilePath -> IO Cleanup
drawImage Internal.DrawImageOptions {..} path = do
    mbPlace <- place <$> getTerminalSize
    icat $
        ["--place=" ++ show p | Just p <- [mbPlace]] ++
        ["--align=center", path]
    print mbPlace
    pure $ icat ["--clear"]
  where
    icat args = do
        (Just inh, _, _, ph) <- Process.createProcess (Process.proc "kitty"
            ("+kitten" : "icat" : "--transfer-mode=stream" : "--stdin=no" : args))
            { Process.std_in = Process.CreatePipe
            }
        IO.hClose inh
        void $ Process.waitForProcess ph

    place :: Size -> Maybe Place
    place Size {..} = do
        (w, h) <- case (dioWidthPercentage, dioHeightPercentage) of
            (Nothing, Nothing) -> Nothing
            (Just w, Nothing)  -> Just (pct w sCols, sRows)
            (Nothing, Just h)  -> Just (sCols, pct h sRows)
            (Just w, Just h)   -> Just (pct w sCols, pct h sRows)
        let x = (sCols - w) `div` 2
            y = (sRows - h) `div` 2
        pure $ Place w h x y


    pct p x = div (x * p) 100
