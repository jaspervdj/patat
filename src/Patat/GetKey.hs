--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}


--------------------------------------------------------------------------------
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif


--------------------------------------------------------------------------------
module Patat.GetKey
    ( Key (..)
    , initialize
    , getKey
    ) where


--------------------------------------------------------------------------------
import qualified System.IO       as IO


--------------------------------------------------------------------------------
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import           Data.Char       (chr)
import           Foreign.C.Types (CInt (..))
#endif


--------------------------------------------------------------------------------
data Key
    = CharKey Char
    | Enter
    | Backspace
    | LeftArrow
    | RightArrow
    | DownArrow
    | UpArrow
    | PageUp
    | PageDown
    | Kill
    | UnknownKey String
    deriving (Eq, Show)


--------------------------------------------------------------------------------
initialize :: IO ()
initialize = IO.hSetBuffering IO.stdin IO.NoBuffering


--------------------------------------------------------------------------------
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import ccall unsafe "conio.h getch" win_getch :: IO CInt
#endif


--------------------------------------------------------------------------------
getKey :: IO Key

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

getKey = do
    c0 <- fromEnum <$> win_getch
    if c0 == 0x00 || c0 == 0xE0 then do
        c1 <- chr . fromEnum <$> win_getch
        case c1 of
            'M' -> return RightArrow
            'K' -> return LeftArrow
            'P' -> return DownArrow
            'H' -> return UpArrow
            'Q' -> return PageDown
            'I' -> return PageUp
            _   -> return $ UnknownKey [chr c0, c1]
    else do
        case chr c0 of
            '\b'   -> return Backspace
            '\r'   -> return Enter
            '\ETX' -> return Kill
            x      -> return $ CharKey x

#else

getKey = do
    c0 <- IO.getChar
    case c0 of
        '\n'   -> return Enter
        '\DEL' -> return Backspace
        '\ESC' -> do
            c1 <- IO.getChar
            case c1 of
                '[' -> do
                    c2 <- IO.getChar
                    case c2 of
                        'C' -> return RightArrow
                        'D' -> return LeftArrow
                        'B' -> return DownArrow
                        'A' -> return UpArrow
                        '6' -> return PageDown
                        '5' -> return PageUp
                        unknown  -> return $ UnknownKey [c0, c1, c2]
                _ -> return $ UnknownKey [c0, c1]
        _ -> return $ CharKey c0

#endif
