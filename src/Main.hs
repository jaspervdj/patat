--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where


--------------------------------------------------------------------------------
import           Patat.Presentation
import           System.Environment (getArgs)
import qualified System.IO          as IO


--------------------------------------------------------------------------------
main :: IO ()
main = do
    (file : _) <- getArgs
    pres       <- readPresentation file

    IO.hSetBuffering IO.stdin IO.NoBuffering
    loop pres

  where
    loop pres0 = do
        displayPresentation pres0
        c       <- readPresentationCommand
        mbPres1 <- updatePresentation c pres0
        case mbPres1 of
            Nothing    -> return ()
            Just pres1 -> loop pres1
