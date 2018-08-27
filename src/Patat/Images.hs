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
import           Patat.Images.Internal
import           Patat.Images.W3m            as W3m
import           Patat.Presentation.Internal


--------------------------------------------------------------------------------
new :: ImageSettings -> IO Handle
new is
    | isType is == "auto" = auto
    | Just (Backend b) <- lookup (isType is) backends =
        case A.fromJSON (A.Object $ isParams is) of
            A.Success c -> b (Explicit c)
            A.Error err -> fail $
                "Patat.Images.new: Error parsing config for " ++
                show (isType is) ++ " image backend: " ++ err
new is = fail $
    "Patat.Images.new: Could not find " ++ show (isType is) ++
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
backends = [("w3m", W3m.backend)]


--------------------------------------------------------------------------------
drawImage :: Handle -> FilePath -> IO ()
drawImage = hDrawImage
