--------------------------------------------------------------------------------
{-# LANGUAGE CPP                      #-}


--------------------------------------------------------------------------------
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
{-# LANGUAGE ForeignFunctionInterface #-}
#endif


--------------------------------------------------------------------------------
module Patat.GetKey
    ( initialize
    , getKey
    ) where


--------------------------------------------------------------------------------
import qualified System.IO       as IO


--------------------------------------------------------------------------------
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import           Data.Char       (chr)
import           Foreign.C.Types (CInt)
#endif


--------------------------------------------------------------------------------
initialize :: IO ()
initialize = IO.hSetBuffering IO.stdin IO.NoBuffering


--------------------------------------------------------------------------------
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
foreign import ccall unsafe "conio.h getch" win_getch :: IO CInt
#endif


--------------------------------------------------------------------------------
getKeyCode :: IO Char
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
getKeyCode = fmap (chr . fromEnum) win_getch
#else
getKeyCode = getChar
#endif


--------------------------------------------------------------------------------
getKey :: IO String
getKey = do
    c0 <- getKeyCode
    case c0 of
        '\ESC' -> do
            c1 <- getKeyCode
            case c1 of
                '[' -> do
                    c2 <- getKeyCode
                    return [c0, c1, c2]
                _ -> return [c0, c1]
        _ -> return [c0]
