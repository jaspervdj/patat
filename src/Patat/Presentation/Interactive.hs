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
import qualified Patat.GetKey                as GetKey
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
    | UnknownCommand GetKey.Key


--------------------------------------------------------------------------------
readPresentationCommand :: IO PresentationCommand
readPresentationCommand = do
    k <- GetKey.getKey
    case k of
        GetKey.Kill        -> return Exit
        GetKey.CharKey 'q' -> return Exit
        GetKey.Enter       -> return Forward
        GetKey.Backspace   -> return Backward
        GetKey.CharKey 'h' -> return Backward
        GetKey.CharKey 'j' -> return SkipForward
        GetKey.CharKey 'k' -> return SkipBackward
        GetKey.CharKey 'l' -> return Forward
        -- Arrow keys
        GetKey.RightArrow  -> return Forward
        GetKey.LeftArrow   -> return Backward
        GetKey.DownArrow   -> return SkipForward
        GetKey.UpArrow     -> return SkipBackward
        -- PageUp and PageDown
        GetKey.PageDown    -> return Forward
        GetKey.PageUp      -> return Backward
        GetKey.CharKey '0' -> return First
        GetKey.CharKey 'G' -> return Last
        GetKey.CharKey 'r' -> return Reload
        GetKey.CharKey    _ -> return (UnknownCommand k)
        GetKey.UnknownKey _ -> return (UnknownCommand k)


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
