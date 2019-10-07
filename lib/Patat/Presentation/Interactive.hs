--------------------------------------------------------------------------------
-- | Module that allows the user to interact with the presentation
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Presentation.Interactive
    ( PresentationCommand (..)
    , readPresentationCommand

    , UpdatedPresentation (..)
    , updatePresentation
    ) where


--------------------------------------------------------------------------------
import           Data.Char                   (isDigit)
import           Patat.Presentation.Internal
import           Patat.Presentation.Read
import qualified System.IO                   as IO
import           Text.Read                   (readMaybe)


--------------------------------------------------------------------------------
data PresentationCommand
    = Exit
    | Forward
    | Backward
    | SkipForward
    | SkipBackward
    | First
    | Last
    | Reload
    | Seek Int
    | UnknownCommand String
    deriving (Eq, Show)


--------------------------------------------------------------------------------
readPresentationCommand :: IO.Handle -> IO PresentationCommand
readPresentationCommand h = do
    k <- readKeys
    case k of
        "q"                       -> return Exit
        "\n"                      -> return Forward
        "\DEL"                    -> return Backward
        "h"                       -> return Backward
        "j"                       -> return SkipForward
        "k"                       -> return SkipBackward
        "l"                       -> return Forward
        -- Arrow keys
        "\ESC[C"                  -> return Forward
        "\ESC[D"                  -> return Backward
        "\ESC[B"                  -> return SkipForward
        "\ESC[A"                  -> return SkipBackward
        -- PageUp and PageDown
        "\ESC[6"                  -> return Forward
        "\ESC[5"                  -> return Backward
        "0"                       -> return First
        "G"                       -> return Last
        "r"                       -> return Reload
        -- Number followed by enter
        _ | Just n <- readMaybe k -> return (Seek n)
        _                         -> return (UnknownCommand k)
  where
    readKeys :: IO String
    readKeys = do
        c0 <- IO.hGetChar h
        case c0 of
            '\ESC' -> do
                c1 <- IO.hGetChar h
                case c1 of
                    '[' -> do
                        c2 <- IO.hGetChar h
                        return [c0, c1, c2]
                    _ -> return [c0, c1]

            _ | isDigit c0 && c0 /= '0' -> (c0 :) <$> readDigits

            _ -> return [c0]

    readDigits :: IO String
    readDigits = do
        c <- IO.hGetChar h
        if isDigit c then (c :) <$> readDigits else return [c]


--------------------------------------------------------------------------------
data UpdatedPresentation
    = UpdatedPresentation !Presentation
    | ExitedPresentation
    | ErroredPresentation String
    deriving (Show)


--------------------------------------------------------------------------------
updatePresentation
    :: PresentationCommand -> Presentation -> IO UpdatedPresentation

updatePresentation cmd presentation = case cmd of
    Exit             -> return ExitedPresentation
    Forward          -> return $ goToSlide $ \(s, f) -> (s, f + 1)
    Backward         -> return $ goToSlide $ \(s, f) -> (s, f - 1)
    SkipForward      -> return $ goToSlide $ \(s, _) -> (s + 10, 0)
    SkipBackward     -> return $ goToSlide $ \(s, _) -> (s - 10, 0)
    First            -> return $ goToSlide $ \_ -> (0, 0)
    Last             -> return $ goToSlide $ \_ -> (numSlides presentation, 0)
    Seek n           -> return $ goToSlide $ \_ -> (n - 1, 0)
    Reload           -> reloadPresentation
    UnknownCommand _ -> return (UpdatedPresentation presentation)
  where
    numSlides :: Presentation -> Int
    numSlides pres = length (pSlides pres)

    clip :: Index -> Presentation -> Index
    clip (slide, fragment) pres
        | slide    >= numSlides pres = (numSlides pres - 1, lastFragments - 1)
        | slide    <  0              = (0, 0)
        | fragment >= numFragments' slide =
            if slide + 1 >= numSlides pres
                then (slide, lastFragments - 1)
                else (slide + 1, 0)
        | fragment < 0 =
            if slide - 1 >= 0
                then (slide - 1, numFragments' (slide - 1) - 1)
                else (slide, 0)
        | otherwise                  = (slide, fragment)
      where
        numFragments' s = maybe 1 numFragments (getSlide s pres)
        lastFragments   = numFragments' (numSlides pres - 1)

    goToSlide :: (Index -> Index) -> UpdatedPresentation
    goToSlide f = UpdatedPresentation $ presentation
        { pActiveFragment = clip (f $ pActiveFragment presentation) presentation
        }

    reloadPresentation = do
        errOrPres <- readPresentation (pFilePath presentation)
        return $ case errOrPres of
            Left  err  -> ErroredPresentation err
            Right pres -> UpdatedPresentation $ pres
                { pActiveFragment = clip (pActiveFragment presentation) pres
                }
