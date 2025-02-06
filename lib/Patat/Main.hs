--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Patat.Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent               (forkIO, threadDelay)
import qualified Control.Concurrent.Async         as Async
import           Control.Concurrent.Chan.Extended (Chan)
import qualified Control.Concurrent.Chan.Extended as Chan
import           Control.Exception                (bracket)
import           Control.Monad                    (forever, unless, void, when)
import qualified Data.Aeson.Extended              as A
import           Data.Foldable                    (for_)
import           Data.Functor                     (($>))
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Sequence.Extended           as Seq
import           Data.Version                     (showVersion)
import qualified Options.Applicative              as OA
import qualified Options.Applicative.Help.Pretty  as OA.PP
import           Patat.AutoAdvance
import qualified Patat.EncodingFallback           as EncodingFallback
import qualified Patat.Eval                       as Eval
import qualified Patat.Images                     as Images
import           Patat.Presentation
import qualified Patat.Presentation.Comments      as Comments
import qualified Patat.PrettyPrint                as PP
import           Patat.PrettyPrint.Matrix         (hPutMatrix)
import           Patat.Transition
import qualified Paths_patat
import           Prelude
import qualified System.Console.ANSI              as Ansi
import           System.Directory                 (doesFileExist,
                                                   getModificationTime)
import           System.Environment (lookupEnv)
import           System.Exit                      (exitFailure, exitSuccess)
import qualified System.IO                        as IO
import qualified Text.Pandoc                      as Pandoc


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
            OA.action  "file" <>  -- For bash file completion
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
    desc = OA.PP.vcat
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
data App = App
    { aOptions      :: Options
    , aImages       :: Maybe Images.Handle
    , aSpeakerNotes :: Maybe Comments.SpeakerNotesHandle
    , aCommandChan  :: Chan AppCommand
    , aPresentation :: Presentation
    , aView         :: AppView
    }


--------------------------------------------------------------------------------
data AppView
    = PresentationView
    | ErrorView String
    | TransitionView TransitionInstance


--------------------------------------------------------------------------------
data AppCommand = PresentationCommand PresentationCommand | TransitionTick TransitionId


--------------------------------------------------------------------------------
main :: IO ()
main = do
    options <- OA.customExecParser parserPrefs parserInfo

    when (oVersion options) $ do
        putStrLn $ showVersion Paths_patat.version
        putStrLn $ "Using pandoc: " ++ showVersion Pandoc.pandocVersion
        exitSuccess

    filePath <- case oFilePath options of
        Just fp -> return fp
        Nothing -> OA.handleParseResult $ OA.Failure $
            OA.parserFailure parserPrefs parserInfo
            (OA.ShowHelpText Nothing) mempty

    errOrPres <- readPresentation zeroVarGen filePath
    pres      <- either (errorAndExit . return) return errOrPres
    let settings = pSettings pres

    unless (oForce options) assertAnsiFeatures

    if oDump options then
        EncodingFallback.withHandle IO.stdout (pEncodingFallback pres) $ do
        Eval.evalAllVars pres >>= dumpPresentation
    else
        -- (Maybe) initialize images backend.
        withMaybeHandle Images.withHandle (psImages settings) $ \images ->

        -- (Maybe) initialize speaker notes.
        withMaybeHandle Comments.withSpeakerNotesHandle
            (psSpeakerNotes settings) $ \speakerNotes ->

        -- Read presentation commands
        interactively (readPresentationCommand) $ \commandChan0 ->

        -- If an auto delay is set, use 'autoAdvance' to create a new one.
        maybeAutoAdvance
            (A.unFlexibleNum <$> psAutoAdvanceDelay settings)
            commandChan0 $ \commandChan1 ->

        -- Change to AppCommand
        Chan.withMapChan PresentationCommand commandChan1 $ \commandChan ->

        -- Spawn a thread that adds 'Reload' commands based on the file time.
        withWatcher (oWatch options) commandChan (pFilePath pres)
            (PresentationCommand Reload) $

        loop App
            { aOptions      = options
            , aImages       = images
            , aSpeakerNotes = speakerNotes
            , aCommandChan  = commandChan
            , aPresentation = pres
            , aView         = PresentationView
            }


--------------------------------------------------------------------------------
loop :: App -> IO ()
loop app@App {..} = do
    for_ aSpeakerNotes $ \sn -> Comments.writeSpeakerNotes sn
        (pEncodingFallback aPresentation)
        (activeSpeakerNotes aPresentation)

    -- Start necessary eval blocks
    presentation <- Eval.evalActiveVars
        (\v -> Chan.writeChan aCommandChan . PresentationCommand . UpdateVar v)
        aPresentation

    size <- getPresentationSize presentation
    Ansi.clearScreen
    Ansi.setCursorPosition 0 0
    cleanup <- case aView of
        PresentationView -> case displayPresentation size presentation of
            DisplayDoc doc    -> drawDoc doc
            DisplayImage path -> drawImg size path
        ErrorView err -> drawDoc $
                displayPresentationError size presentation err
        TransitionView tr -> do
            drawMatrix (tiSize tr) . fst . NonEmpty.head $ tiFrames tr
            pure mempty

    appCmd <- Chan.readChan aCommandChan
    cleanup
    case appCmd of
        TransitionTick eid -> case aView of
            PresentationView -> loop app
            ErrorView _      -> loop app
            TransitionView tr0  -> case stepTransition eid tr0 of
                Just tr1 -> do
                    scheduleTransitionTick tr1
                    loop app {aView = TransitionView tr1}
                Nothing -> loop app {aView = PresentationView}
        PresentationCommand c -> do
            update <- updatePresentation c presentation
            case update of
                ExitedPresentation       -> return ()
                UpdatedPresentation pres
                    | Just tgen <- mbTransition c size presentation pres -> do
                        tr <- tgen
                        scheduleTransitionTick tr
                        loop app
                            {aPresentation = pres, aView = TransitionView tr}
                    | otherwise -> loop app
                        {aPresentation = pres, aView = PresentationView}
                ErroredPresentation err  ->
                    loop app {aView = ErrorView err}
  where
    drawDoc doc = EncodingFallback.withHandle
        IO.stdout (pEncodingFallback aPresentation) $
        PP.putDoc doc $> mempty
    drawImg size path = case aImages of
        Nothing -> drawDoc $ displayPresentationError
            size aPresentation "image backend not initialized"
        Just img -> do
            putStrLn ""
            IO.hFlush IO.stdout
            Images.drawImage img path
    drawMatrix size raster = hPutMatrix IO.stdout size raster

    mbTransition c size old new
        | c == Forward
        , oldSlide + 1 == newSlide
        , DisplayDoc oldDoc <- displayPresentation size old
        , DisplayDoc newDoc <- displayPresentation size new
        , Just (Just tgen) <- pTransitionGens new `Seq.safeIndex` newSlide =
            Just $ newTransition tgen size oldDoc newDoc
        | otherwise = Nothing
      where
        (oldSlide, _) = pActiveFragment old
        (newSlide, _) = pActiveFragment new

    scheduleTransitionTick tr = void $ forkIO $ do
        threadDelayDuration . snd . NonEmpty.head $ tiFrames tr
        Chan.writeChan aCommandChan $ TransitionTick $ tiId tr


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
interactively reader app = bracket setup teardown $ \(_, _, chan) ->
    Async.withAsync
        (forever $ reader IO.stdin >>= Chan.writeChan chan)
        (\_ -> app chan)
  where
    setup = do
        chan <- Chan.newChan
        echo <- IO.hGetEcho      IO.stdin
        buff <- IO.hGetBuffering IO.stdin
        IO.hSetEcho      IO.stdin False
        IO.hSetBuffering IO.stdin IO.NoBuffering

        -- Suppress cursor hiding for WezTerm image compatibility
        termProgram <- lookupEnv "TERM_PROGRAM"
        unless (termProgram == Just "WezTerm") $ Ansi.hideCursor

        return (echo, buff, chan)

    teardown (echo, buff, _chan) = do
        Ansi.showCursor
        Ansi.clearScreen
        Ansi.setCursorPosition 0 0
        IO.hSetEcho      IO.stdin echo
        IO.hSetBuffering IO.stdin buff


--------------------------------------------------------------------------------
withWatcher
    :: Bool -> Chan.Chan cmd -> FilePath -> cmd -> IO a -> IO a
withWatcher False _    _        _   mx = mx
withWatcher True  chan filePath cmd mx = do
    mtime0 <- getModificationTime filePath
    Async.withAsync (watcher mtime0) (\_ -> mx)
  where
    watcher mtime0 = do
        -- The extra exists check helps because some editors temporarily make
        -- the file disappear while writing.
        exists <- doesFileExist filePath
        mtime1 <- if exists then getModificationTime filePath else return mtime0

        when (mtime1 > mtime0) $ Chan.writeChan chan cmd
        threadDelay (200 * 1000)
        watcher mtime1


--------------------------------------------------------------------------------
-- | Wrapper for optional handles.
withMaybeHandle
    :: (settings -> (handle -> IO a) -> IO a)
    -> Maybe settings
    -> (Maybe handle -> IO a)
    -> IO a
withMaybeHandle _    Nothing         f = f Nothing
withMaybeHandle impl (Just settings) f = impl settings (f . Just)
