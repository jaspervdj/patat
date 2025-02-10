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
    Internal.withEscapeSequence $ do
        putStr "1337;File=inline=1;doNotMoveCursor=1;"
        case decodeImage content of
            Left _ -> pure () 
            Right img -> putStr $ getAspectRatio img
        putStr ":"
        B.putStr (B64.encode content)
    return mempty


--------------------------------------------------------------------------------
getAspectRatio  :: DynamicImage -> String
getAspectRatio i | go_w i / go_h i < (1 :: Double) = "width=auto;height=95%;"
                 | otherwise                       = "width=100%;height=auto;"
    where
          go_h = fromIntegral . (dynamicMap imageHeight)
          go_w = fromIntegral . (dynamicMap imageWidth)
