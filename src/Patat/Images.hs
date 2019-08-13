--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Patat.Images
    ( Backend
    , Handle
    , new
    , drawImage
    ) where


--------------------------------------------------------------------------------
import           Control.Exception           (catch)
import qualified Data.Aeson                  as A
import qualified Data.Text                   as T
import           Patat.Cleanup
import           Patat.Images.Internal
import qualified Patat.Images.ITerm2         as ITerm2
import qualified Patat.Images.W3m            as W3m
import           Patat.Presentation.Internal


--------------------------------------------------------------------------------
new :: ImageSettings -> IO Handle
new is
    | isBackend is == "auto" = auto
    | Just (Backend b) <- lookup (isBackend is) backends =
        case A.fromJSON (A.Object $ isParams is) of
            A.Success c -> b (Explicit c)
            A.Error err -> fail $
                "Patat.Images.new: Error parsing config for " ++
                show (isBackend is) ++ " image backend: " ++ err
new is = fail $
    "Patat.Images.new: Could not find " ++ show (isBackend is) ++
    " image backend."


--------------------------------------------------------------------------------
auto :: IO Handle
auto = go [] backends
  where
    go names ((name, Backend b) : bs) = catch
        (b Auto)
        (\(BackendNotSupported _) -> go (name : names) bs)
    go names [] = fail $
        "Could not find a supported backend, tried: " ++
        T.unpack (T.intercalate ", " (reverse names))


--------------------------------------------------------------------------------
-- | All supported backends.  We can use CPP to include or exclude some
-- depending on platform availability.
backends :: [(T.Text, Backend)]
backends =
    [ ("iterm2", ITerm2.backend)
    , ("w3m",    W3m.backend)
    ]


--------------------------------------------------------------------------------
drawImage :: Handle -> FilePath -> IO Cleanup
drawImage = hDrawImage
