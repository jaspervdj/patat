--------------------------------------------------------------------------------
module Patat.TerminalInfo
    ( TerminalInfo (..)
    , prettyTerminalInfo
    , detectTerminalInfo
    ) where


--------------------------------------------------------------------------------
import           Data.List          (isInfixOf, isPrefixOf)
import           Data.Maybe         (isJust)
import           System.Environment (lookupEnv)


--------------------------------------------------------------------------------
data TerminalInfo = TerminalInfo
    { tiTerminal :: Terminal
    , tiTmux     :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
data Terminal
    = Alacritty
    | ITerm2
    | Kitty
    | WezTerm
    | Unknown
    deriving (Show)


--------------------------------------------------------------------------------
prettyTerminalInfo :: TerminalInfo -> String
prettyTerminalInfo ti =
    show (tiTerminal ti) ++ if tiTmux ti then " (tmux)" else ""


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
