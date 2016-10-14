--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where


--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent           (forkIO, threadDelay)
import qualified Control.Concurrent.Chan      as Chan
import           Control.Monad                (forever, unless, when)
import           Data.Monoid                  ((<>))
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import qualified Patat.GetKey                 as GetKey
import           Patat.Presentation
import qualified Paths_patat
import qualified System.Console.ANSI          as Ansi
import           System.Directory             (getModificationTime)
import           System.Exit                  (exitFailure)
import qualified System.IO                    as IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Prelude


--------------------------------------------------------------------------------
data Options = Options
    { oFilePath :: !FilePath
    , oForce    :: !Bool
    , oDump     :: !Bool
    , oWatch    :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.strArgument $
            OA.metavar "FILENAME" <>
            OA.help    "Input file")
    <*> (OA.switch $
            OA.long    "force" <>
            OA.short   'f' <>
            OA.help    "Force ANSI terminal" <>
            OA.hidden)
    <*> (OA.switch $
            OA.long    "dump" <>
            OA.short   'd' <>
            OA.help    "Just dump all slides and exit" <>
            OA.hidden)
    <*> (OA.switch $
            OA.long    "watch" <>
            OA.short   'w' <>
            OA.help    "Watch file for changes")


--------------------------------------------------------------------------------
parserInfo :: OA.ParserInfo Options
parserInfo = OA.info (OA.helper <*> parseOptions) $
    OA.fullDesc <>
    OA.header ("patat v" <> showVersion Paths_patat.version) <>
    OA.progDescDoc (Just desc)
  where
    desc = PP.vcat
        [ "Terminal-based presentations using Pandoc"
        , ""
        , "Controls:"
        , "- Next slide:             space, enter, l, right"
        , "- Previous slide:         backspace, h, left"
        , "- Go forward 10 slides:   j, down"
        , "- Go backward 10 slides:  k, up"
        , "- First slide:            0"
        , "- Last slide:             G"
        , "- Reload file:            r"
        , "- Quit:                   q"
        ]


--------------------------------------------------------------------------------
errorAndExit :: [String] -> IO a
errorAndExit msg = do
    mapM_ (IO.hPutStrLn IO.stderr) msg
    exitFailure


--------------------------------------------------------------------------------
assertAnsiFeatures :: IO ()
assertAnsiFeatures = do
    supports <- Ansi.hSupportsANSI IO.stdout
    unless supports $ errorAndExit
        [ "It looks like your terminal does not support ANSI codes."
        , "If you still want to run the presentation, use `--force`."
        ]


--------------------------------------------------------------------------------
main :: IO ()
main = do
    options   <- OA.customExecParser (OA.prefs OA.showHelpOnError) parserInfo
    errOrPres <- readPresentation (oFilePath options)
    pres      <- either (errorAndExit . return) return errOrPres

    unless (oForce options) assertAnsiFeatures

    if oDump options
        then dumpPresentation pres
        else interactiveLoop options pres

  where
    interactiveLoop options pres0 = do
        GetKey.initialize
        commandChan <- Chan.newChan

        _ <- forkIO $ forever $
            readPresentationCommand >>= Chan.writeChan commandChan

        mtime0 <- getModificationTime (pFilePath pres0)
        let watcher mtime = do
                mtime' <- getModificationTime (pFilePath pres0)
                when (mtime' > mtime) $ Chan.writeChan commandChan Reload
                threadDelay (200 * 1000)
                watcher mtime'

        when (oWatch options) $ do
            _ <- forkIO $ watcher mtime0
            return ()

        let loop pres = do
                displayPresentation pres
                c      <- Chan.readChan commandChan
                update <- updatePresentation c pres
                case update of
                    ExitedPresentation        -> return ()
                    UpdatedPresentation pres' -> loop pres'
                    ErroredPresentation err   -> errorAndExit [err]

        loop pres0
