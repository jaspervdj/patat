--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Patat.TerminalInfo
    ( TerminalInfo (..)
    , prettyTerminalInfo
    , detectTerminalInfo
    ) where


--------------------------------------------------------------------------------
import qualified Data.Aeson.Extended as A
import           Data.List           (isInfixOf, isPrefixOf)
import           Data.Maybe          (isJust)
import qualified Data.Text           as T
import           System.Environment  (lookupEnv)


--------------------------------------------------------------------------------
data TerminalInfo = TerminalInfo
    { tiTerm :: Terminal
    , tiTmux :: Bool
    -- Move size here?
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
instance A.FromJSON TerminalInfo where
    parseJSON = A.withObject "FromJSON TerminalInfo" $ \o -> TerminalInfo
        <$> o A..:? "term" A..!= Unknown
        <*> o A..:? "tmux" A..!= False


--------------------------------------------------------------------------------
data Terminal
    = Alacritty
    | ITerm2
    | Kitty
    | WezTerm
    | Unknown
    deriving (Bounded, Enum, Eq, Show)


--------------------------------------------------------------------------------
instance A.FromJSON Terminal where
    parseJSON = A.withText "FromJSON Terminal" $ \t ->
        maybe (fail $ "unknown terminal: " ++ T.unpack t) pure $ parseTerminal t


--------------------------------------------------------------------------------
prettyTerminal :: Terminal -> T.Text
prettyTerminal terminal = case terminal of
    Alacritty -> "alacritty"
    ITerm2    -> "iterm2"
    Kitty     -> "kitty"
    WezTerm   -> "wezterm"
    Unknown   -> "unknown"


--------------------------------------------------------------------------------
parseTerminal :: T.Text -> Maybe Terminal
parseTerminal txt =
    lookup txt [(prettyTerminal t, t) | t <- [minBound .. maxBound]]


--------------------------------------------------------------------------------
prettyTerminalInfo :: TerminalInfo -> String
prettyTerminalInfo ti =
    show (tiTerm ti) ++ if tiTmux ti then " (tmux)" else ""


--------------------------------------------------------------------------------
detectTerminalInfo :: IO TerminalInfo
detectTerminalInfo = TerminalInfo <$> detectTerminal <*> detectTmux


--------------------------------------------------------------------------------
detectTerminal :: IO Terminal
detectTerminal = do
    term <- lookupEnv "TERM"
    termProgram <- lookupEnv "TERM_PROGRAM"
    let termIs k = Just k == term || Just k == termProgram

        detectors =
            [ (Alacritty, pure $ termIs "alacritty")
            , (WezTerm, pure $ termIs "WezTerm")
            , (ITerm2, pure $ termIs "iTerm.app")
            , (Kitty, pure $ maybe False ("kitty" `isInfixOf`) term)
            , (Alacritty, isJust <$> lookupEnv "ALACRITTY_WINDOW_ID")
            ]

        go [] = pure Unknown
        go ((t, mp) : ds) = do
            p <- mp
            if p then pure t else go ds

    go detectors


--------------------------------------------------------------------------------
detectTmux :: IO Bool
detectTmux = maybe False ("screen" `isPrefixOf`) <$> lookupEnv "TERM"
