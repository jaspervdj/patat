--------------------------------------------------------------------------------
-- | Module that allows the user to interact with the presentation
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Presentation.Interactive
    ( PresentationCommand
    , readPresentationCommand

    , UpdatedPresentation (..)
    , updatePresentation
    ) where


--------------------------------------------------------------------------------
import           Patat.Presentation.Internal
import           Patat.Presentation.Read


--------------------------------------------------------------------------------
type PresentationCommand = String


--------------------------------------------------------------------------------
readPresentationCommand :: IO PresentationCommand
readPresentationCommand = do
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

updatePresentation char presentation = case char of
    "q"      -> return ExitedPresentation
    "\n"     -> return $ goToSlide fwd
    "\DEL"   -> return $ goToSlide bwd
    "h"      -> return $ goToSlide bwd
    "j"      -> return $ goToSlide skipFwd
    "k"      -> return $ goToSlide skipBwd
    "l"      -> return $ goToSlide fwd
    "\ESC[C" -> return $ goToSlide fwd      -- Right arrow
    "\ESC[D" -> return $ goToSlide bwd      -- Left arrow
    "\ESC[B" -> return $ goToSlide skipFwd  -- Down arrow
    "\ESC[A" -> return $ goToSlide skipBwd  -- Up arrow
    "0"      -> return $ goToSlide (const 0)
    "G"      -> return $ goToSlide (const $ numSlides - 1)
    "r"      -> reloadPresentation
    _        -> return $ UpdatedPresentation presentation
  where
    numSlides = length (pSlides presentation)
    clip idx  = min (max 0 idx) (numSlides - 1)

    fwd       = \x -> x + 1
    bwd       = \x -> x - 1
    skipFwd   = \x -> x + 10
    skipBwd   = \x -> x - 10

    goToSlide f = UpdatedPresentation $
        presentation {pActiveSlide = clip (f $ pActiveSlide presentation)}

    reloadPresentation = do
        errOrPres <- readPresentation (pFilePath presentation)
        return $ case errOrPres of
            Left  err  -> ErroredPresentation err
            Right pres -> UpdatedPresentation $
                pres {pActiveSlide = clip (pActiveSlide presentation)}
