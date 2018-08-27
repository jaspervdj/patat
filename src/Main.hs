--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where


--------------------------------------------------------------------------------
import           Control.Applicative          ((<$>), (<*>))
import           Control.Concurrent           (forkIO, threadDelay)
import qualified Control.Concurrent.Chan      as Chan
import           Control.Exception            (finally)
import           Control.Monad                (forever, unless, when)
import qualified Data.Aeson.Extended          as A
import           Data.Monoid                  (mempty, (<>))
import           Data.Time                    (UTCTime)
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import           Patat.AutoAdvance
import qualified Patat.Images                 as Images
import           Patat.Presentation
import qualified Paths_patat
import           Prelude
import qualified System.Console.ANSI          as Ansi
import           System.Directory             (doesFileExist,
                                               getModificationTime)
import           System.Exit                  (exitFailure, exitSuccess)
import qualified System.IO                    as IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP


--------------------------------------------------------------------------------
data Options = Options
    { oFilePath :: !(Maybe FilePath)
    , oForce    :: !Bool
    , oDump     :: !Bool
    , oWatch    :: !Bool
    , oVersion  :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.optional $ OA.strArgument $
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
    <*> (OA.switch $
            OA.long    "version" <>
            OA.help    "Display version info and exit" <>
            OA.hidden)


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
        , "- Next slide:             space, enter, l, right, pagedown"
        , "- Previous slide:         backspace, h, left, pageup"
        , "- Go forward 10 slides:   j, down"
        , "- Go backward 10 slides:  k, up"
        , "- First slide:            0"
        , "- Last slide:             G"
        , "- Reload file:            r"
        , "- Quit:                   q"
        ]


--------------------------------------------------------------------------------
parserPrefs :: OA.ParserPrefs
parserPrefs = OA.prefs OA.showHelpOnError


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
    options <- OA.customExecParser parserPrefs parserInfo

    when (oVersion options) $ do
        putStrLn (showVersion Paths_patat.version)
        exitSuccess

    filePath <- case oFilePath options of
        Just fp -> return fp
        Nothing -> OA.handleParseResult $ OA.Failure $
            OA.parserFailure parserPrefs parserInfo OA.ShowHelpText mempty

    errOrPres <- readPresentation filePath
    pres      <- either (errorAndExit . return) return errOrPres

    unless (oForce options) assertAnsiFeatures

    -- (Maybe) initialize images backend.
    images <- traverse Images.new (psImages $ pSettings pres)

    if oDump options
        then dumpPresentation pres
        else interactiveLoop options images pres
  where
    interactiveLoop :: Options -> Maybe Images.Handle -> Presentation -> IO ()
    interactiveLoop options images pres0 = (`finally` cleanup) $ do
        IO.hSetBuffering IO.stdin IO.NoBuffering
        Ansi.hideCursor

        -- Spawn the initial channel that gives us commands based on user input.
        commandChan0 <- Chan.newChan
        _            <- forkIO $ forever $
            readPresentationCommand >>= Chan.writeChan commandChan0

        -- If an auto delay is set, use 'autoAdvance' to create a new one.
        commandChan <- case psAutoAdvanceDelay (pSettings pres0) of
            Nothing                    -> return commandChan0
            Just (A.FlexibleNum delay) -> autoAdvance delay commandChan0

        -- Spawn a thread that adds 'Reload' commands based on the file time.
        mtime0 <- getModificationTime (pFilePath pres0)
        when (oWatch options) $ do
            _ <- forkIO $ watcher commandChan (pFilePath pres0) mtime0
            return ()

        let loop :: Presentation -> Maybe String -> IO ()
            loop pres mbError = do
                case mbError of
                    Nothing  -> displayPresentation images pres
                    Just err -> displayPresentationError pres err

                c      <- Chan.readChan commandChan
                update <- updatePresentation c pres
                case update of
                    ExitedPresentation        -> return ()
                    UpdatedPresentation pres' -> loop pres' Nothing
                    ErroredPresentation err   -> loop pres (Just err)

        loop pres0 Nothing

    cleanup :: IO ()
    cleanup = do
        Ansi.showCursor
        Ansi.clearScreen
        Ansi.setCursorPosition 0 0


--------------------------------------------------------------------------------
watcher :: Chan.Chan PresentationCommand -> FilePath -> UTCTime -> IO a
watcher chan filePath mtime0 = do
    -- The extra exists check helps because some editors temporarily make the
    -- file disappear while writing.
    exists <- doesFileExist filePath
    mtime1 <- if exists then getModificationTime filePath else return mtime0

    when (mtime1 > mtime0) $ Chan.writeChan chan Reload
    threadDelay (200 * 1000)
    watcher chan filePath mtime1
