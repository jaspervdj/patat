--------------------------------------------------------------------------------
-- | Module that allows the user to interact with the presentation
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Presentation.Interactive
    ( PresentationCommand
    , readPresentationCommand
    , updatePresentation
    ) where


--------------------------------------------------------------------------------
import           Patat.Presentation.Internal
import           Patat.Presentation.Read
import           System.Environment          (getArgs)
import qualified System.IO                   as IO
import qualified Text.Pandoc                 as Pandoc


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
updatePresentation
    :: PresentationCommand -> Presentation -> IO (Maybe Presentation)

updatePresentation char presentation = case char of
    "q"      -> return Nothing
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
    _        -> return $ Just presentation
  where
    numSlides = length (pSlides presentation)
    clip idx  = min (max 0 idx) (numSlides - 1)

    fwd       = \x -> x + 1
    bwd       = \x -> x - 1
    skipFwd   = \x -> x + 10
    skipBwd   = \x -> x - 10

    goToSlide f =
        Just presentation {pActiveSlide = clip (f $ pActiveSlide presentation)}

    reloadPresentation = do
        pres <- readPresentation (pFilePath presentation)
        return $ Just pres {pActiveSlide = clip (pActiveSlide presentation)}
