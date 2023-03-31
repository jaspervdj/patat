--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent           (forkIO, killThread, threadDelay)
import           Control.Concurrent.Chan      (Chan)
import qualified Control.Concurrent.Chan      as Chan
import           Control.Exception            (bracket)
import           Control.Monad                (forever, unless, when, forM_)
import qualified Data.Aeson.Extended          as A
import           Data.Functor                 (($>))
import qualified Data.Text                    as T
import           Data.Time                    (UTCTime)
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import           Patat.AutoAdvance
import qualified Patat.Images                 as Images
import           Patat.Presentation
import qualified Patat.PrettyPrint            as PP
import qualified Paths_patat
import           Prelude
import qualified System.Console.ANSI          as Ansi
import           System.Directory             (doesFileExist,
                                               getModificationTime)
import           System.Exit                  (exitFailure, exitSuccess)
import qualified System.IO                    as IO
import qualified Text.Pandoc                  as Pandoc
import qualified Text.PrettyPrint.ANSI.Leijen as PPL


--------------------------------------------------------------------------------
data Options = Options
    { oFilePath :: !([FilePath])
    , oForce    :: !Bool
    , oDump     :: !Bool
    , oWatch    :: !Bool
    , oVersion  :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.many $ OA.strArgument $
            OA.metavar "FILENAMES" <>
            OA.help    "Input files")
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
    desc = PPL.vcat
        [ "Terminal-based presentations using Pandoc"
        , ""
        , "Controls:"
        , "- Next slide:             space, enter, l, right, pagedown"
        , "- Previous slide:         backspace, h, left, pageup"
        , "- Go forward 10 slides:   j, down"
        , "- Go backward 10 slides:  k, up"
        , "- First slide:            0"
        , "- Last slide:             G"
        , "- Jump to slide N:        N followed by enter"
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
present :: Bool -> Presentation -> IO ()
present watchChanges presentation =
  traverse Images.new (psImages $ pSettings presentation) >>= \images ->
    interactively readPresentationCommand $ \commandChan0 -> do
      -- If an auto delay is set, use 'autoAdvance' to create a new one.
      commandChan <- case psAutoAdvanceDelay (pSettings presentation) of
        Nothing -> return commandChan0
        Just (A.FlexibleNum delay) -> autoAdvance delay commandChan0

      -- Spawn a thread that adds 'Reload' commands based on the file time.
      mtime0 <- getModificationTime (pFilePath presentation)
      when watchChanges $ do
        _ <- forkIO $ watcher commandChan (pFilePath presentation) mtime0
        return ()

      let loop :: Presentation -> Maybe String -> IO ()
          loop pres mbError = do
            size <- getDisplaySize pres
            let display = case mbError of
                  Nothing -> displayPresentation size pres
                  Just err ->
                    DisplayDoc $
                      displayPresentationError size pres err

            Ansi.clearScreen
            Ansi.setCursorPosition 0 0
            cleanup <- case display of
              DisplayDoc doc -> PP.putDoc doc $> mempty
              DisplayImage path -> case images of
                Nothing -> do
                  PP.putDoc $
                    displayPresentationError
                      size
                      pres
                      "image backend not initialized"
                  pure mempty
                Just img -> do
                  putStrLn ""
                  IO.hFlush IO.stdout
                  Images.drawImage img path

            c <- Chan.readChan commandChan
            update <- updatePresentation c pres
            cleanup
            case update of
              ExitedPresentation -> return ()
              UpdatedPresentation pres' -> loop pres' Nothing
              ErroredPresentation err -> loop pres (Just err)

      loop presentation Nothing

--------------------------------------------------------------------------------
main :: IO ()
main = do
    options <- OA.customExecParser parserPrefs parserInfo

    when (oVersion options) $ do
        putStrLn (showVersion Paths_patat.version)
        putStrLn $ "Using pandoc: " ++ T.unpack Pandoc.pandocVersion
        exitSuccess

    unless (oForce options) assertAnsiFeatures

    filePaths <- case oFilePath options of
        [] -> OA.handleParseResult $ OA.Failure $
            OA.parserFailure parserPrefs parserInfo
            (OA.ShowHelpText Nothing) mempty
        fp -> return fp
    forM_ filePaths (tryPresent options)
    where 
      tryPresent options filePath = do
        errOrPres <- readPresentation filePath
        pres      <- either (errorAndExit . return) return errOrPres
        if oDump options
            then dumpPresentation pres
            else present (oWatch options) pres

--------------------------------------------------------------------------------
-- | Utility for dealing with pecularities of stdin & interactive applications
-- on the terminal.  Tries to restore the original state of the terminal as much
-- as possible.
interactively
    :: (IO.Handle -> IO a)
    -- ^ Reads a command from stdin (or from some other IO).  This will be
    -- interrupted by 'killThread' when the application finishes.
    -> (Chan a -> IO ())
    -- ^ Application to run.
    -> IO ()
    -- ^ Returns when application finishes.
interactively reader app = bracket setup teardown $ \(_, _, _, chan) -> app chan
  where
    setup = do
        chan <- Chan.newChan
        echo <- IO.hGetEcho      IO.stdin
        buff <- IO.hGetBuffering IO.stdin
        IO.hSetEcho      IO.stdin False
        IO.hSetBuffering IO.stdin IO.NoBuffering
        Ansi.hideCursor
        readerThreadId <- forkIO $ forever $
            reader IO.stdin >>= Chan.writeChan chan
        return (echo, buff, readerThreadId, chan)

    teardown (echo, buff, readerThreadId, _chan) = do
        Ansi.showCursor
        Ansi.clearScreen
        Ansi.setCursorPosition 0 0
        killThread readerThreadId
        IO.hSetEcho      IO.stdin echo
        IO.hSetBuffering IO.stdin buff


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
