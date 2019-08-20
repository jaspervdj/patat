--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Images.ITerm2
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Control.Exception           (throwIO)
import           Control.Monad               (unless, when)
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy        as BL
import qualified Data.List                   as L
import           Patat.Cleanup               (Cleanup)
import qualified Patat.Images.Internal       as Internal
import           System.Environment          (lookupEnv)


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
        termProgram <- lookupEnv "TERM_PROGRAM"
        unless (termProgram == Just "iTerm.app") $ throwIO $
            Internal.BackendNotSupported "TERM_PROGRAM not iTerm.app"

    return Internal.Handle {Internal.hDrawImage = drawImage}


--------------------------------------------------------------------------------
drawImage :: FilePath -> IO Cleanup
drawImage path = do
    content <- BL.readFile path
    withEscapeSequence $ do
        putStr "1337;File=inline=1;width=100%;height=100%:"
        BL.putStr (B64.encode content)
    return mempty


--------------------------------------------------------------------------------
withEscapeSequence :: IO () -> IO ()
withEscapeSequence f = do
    term <- lookupEnv "TERM"
    let inScreen = maybe False ("screen" `L.isPrefixOf`) term
    putStr $ if inScreen then "\ESCPtmux;\ESC\ESC]" else "\ESC]"
    f
    putStrLn $ if inScreen then "\a\ESC\\" else "\a"
