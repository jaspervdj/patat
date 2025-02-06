--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Patat.Images.WezTerm
    ( backend
    ) where


--------------------------------------------------------------------------------
import           Control.Exception           (throwIO)
import           Control.Monad               (unless, when)
import           Codec.Picture
import qualified Data.Aeson                  as A
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString             as B
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
        unless (termProgram == Just "WezTerm") $ throwIO $
            Internal.BackendNotSupported "TERM_PROGRAM not WezTerm"

    return Internal.Handle {Internal.hDrawImage = drawImage}


--------------------------------------------------------------------------------
drawImage :: FilePath -> IO Cleanup
drawImage path = do
    content <- B.readFile path
    image_dim <- return . getAspectRatio . decodeImage $ content
    withEscapeSequence $ do
        putStr "1337;File=inline=1;doNotMoveCursor=1;"
        putStr image_dim
        putStr ":"
        B.putStr (B64.encode content)
    return mempty

--------------------------------------------------------------------------------
getAspectRatio :: Either String DynamicImage -> String
getAspectRatio (Left _) = ""
getAspectRatio (Right i) | go_ar < 1 = "width=auto;height=95%;"
                         | otherwise = "width=100%;height=auto;"
    where go_ar :: Double
          go_ar = go_w i / go_h i
          go_h = fromIntegral . (dynamicMap imageHeight)
          go_w = fromIntegral . (dynamicMap imageWidth)

--------------------------------------------------------------------------------
withEscapeSequence :: IO () -> IO ()
withEscapeSequence f = do
    term <- lookupEnv "TERM"
    let inScreen = maybe False ("screen" `L.isPrefixOf`) term
    putStr $ if inScreen then "\ESCPtmux;\ESC\ESC]" else "\ESC]"
    f
    putStrLn $ if inScreen then "\a\ESC\\" else "\a"
