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
import           Patat.Presentation.Internal
import           Patat.Presentation.Read


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


--------------------------------------------------------------------------------
readPresentationCommand :: IO PresentationCommand
readPresentationCommand = do
    k <- readKey
    case k of
        "q"      -> return Exit
        "\n"     -> return Forward
        "\DEL"   -> return Backward
        "h"      -> return Backward
        "j"      -> return SkipForward
        "k"      -> return SkipBackward
        "l"      -> return Forward
        "\ESC[C" -> return Forward
        "\ESC[D" -> return Backward
        "\ESC[B" -> return SkipForward
        "\ESC[A" -> return SkipBackward
        "0"      -> return First
        "G"      -> return Last
        "r"      -> return Reload
        _        -> readPresentationCommand
  where
    readKey :: IO String
    readKey = do
        c0 <- getChar
        case c0 of
            '\ESC' -> do
                c1 <- getChar
                case c1 of
                    '[' -> do
                        c2 <- getChar
                        return [c0, c1, c2]
                    _ -> return [c0, c1]
            _ -> return [c0]


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
    Exit         -> return ExitedPresentation
    Forward      -> return $ goToSlide (\x -> x + 1)
    Backward     -> return $ goToSlide (\x -> x - 1)
    SkipForward  -> return $ goToSlide (\x -> x + 10)
    SkipBackward -> return $ goToSlide (\x -> x - 10)
    First        -> return $ goToSlide (\_ -> 0)
    Last         -> return $ goToSlide (\_ -> numSlides - 1)
    Reload       -> reloadPresentation
  where
    numSlides = length (pSlides presentation)
    clip idx  = min (max 0 idx) (numSlides - 1)

    goToSlide f = UpdatedPresentation $
        presentation {pActiveSlide = clip (f $ pActiveSlide presentation)}

    reloadPresentation = do
        errOrPres <- readPresentation (pFilePath presentation)
        return $ case errOrPres of
            Left  err  -> ErroredPresentation err
            Right pres -> UpdatedPresentation $
                pres {pActiveSlide = clip (pActiveSlide presentation)}
