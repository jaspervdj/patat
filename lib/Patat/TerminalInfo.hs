--------------------------------------------------------------------------------
module Patat.TerminalInfo
    ( TerminalInfo (..)
    , prettyTerminalInfo
    , detectTerminalInfo
    ) where


--------------------------------------------------------------------------------
import           Data.List          (isPrefixOf)
import           Data.Maybe         (isJust)
import Control.Applicative ((<|>))
import           System.Environment (lookupEnv)


--------------------------------------------------------------------------------
data TerminalInfo = TerminalInfo
    { tiTerminal :: Terminal
    , tiTmux     :: Bool
    } deriving (Show)


--------------------------------------------------------------------------------
data Terminal
    = Alacritty
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
    case (term >>= (`lookup` byTerm)) <|> (termProgram >>= (`lookup` byTerm)) of
        Just t -> pure t
        _ -> go detectors
  where
    byTerm :: [(String, Terminal)]
    byTerm =
        [ ("alacritty", Alacritty)
        , ("WezTerm", WezTerm)
        ]

    detectors :: [(Terminal, IO Bool)]
    detectors =
        [ (Alacritty, isJust <$> lookupEnv "ALACRITTY_WINDOW_ID")
        ]

    go [] = pure Unknown
    go ((t, mp) : ds) = do
        p <- mp
        if p then pure t else go ds


--------------------------------------------------------------------------------
detectTmux :: IO Bool
detectTmux = maybe False ("screen" `isPrefixOf`) <$> lookupEnv "TERM"
