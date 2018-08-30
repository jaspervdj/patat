--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Images.Iterm
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Control.Exception      (throwIO)
import qualified Data.Aeson as A
import qualified Patat.Images.Internal  as Internal
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64.Lazy as B64
import System.Environment (lookupEnv)
import qualified Data.List as L

--------------------------------------------------------------------------------
backend :: Internal.Backend
backend = Internal.Backend new


--------------------------------------------------------------------------------
data Config = Config
instance A.FromJSON Config where parseJSON _ = return Config

--------------------------------------------------------------------------------
new :: Internal.Config Config -> IO Internal.Handle
new config = do
    case config of
        Internal.Explicit _ -> return ()
        Internal.Auto       -> checkEnv

    return Internal.Handle {Internal.hDrawImage = drawImage}
    where
        checkEnv = do
            term <- lookupEnv "TERM_PROGRAM"
            if term == Just "iTerm.app"
               then return ()
               else throwIO $ Internal.BackendNotSupported "TERM_PROGRAM not iTerm.app"


--------------------------------------------------------------------------------
drawImage :: FilePath -> IO ()
drawImage path = do
    content <- BL.readFile path
    let contentb64 = B64.encode content
    withEscapeSequence $ do
        putStr "1337;File=inline=1;width=100%;height=100%:"
        BL.putStr contentb64


withEscapeSequence :: IO () -> IO ()
withEscapeSequence f = do
    term <- lookupEnv "TERM"
    let
        inScreen =
            case term of
                 Nothing -> False
                 Just t -> "screen" `L.isPrefixOf` t

    putStr $ if inScreen then "\ESCPtmux;\ESC\ESC]" else "\ESC]"
    f
    putStrLn $ if inScreen then "\a\ESC\\" else "\a"
