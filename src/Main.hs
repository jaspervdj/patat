--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where


--------------------------------------------------------------------------------
import           Control.Monad                (unless)
import           Data.Monoid                  ((<>))
import           Data.Version                 (showVersion)
import qualified Options.Applicative          as OA
import           Patat.Presentation
import qualified Paths_patat
import qualified System.Console.ANSI          as Ansi
import           System.Exit                  (exitFailure)
import qualified System.IO                    as IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP


--------------------------------------------------------------------------------
data Options = Options
    { oFilePath :: !FilePath
    , oForce    :: !Bool
    , oDump     :: !Bool
    } deriving (Show)


--------------------------------------------------------------------------------
parseOptions :: OA.Parser Options
parseOptions = Options
    <$> (OA.strArgument $
            OA.metavar "FILENAME" <>
            OA.help    "Input file")
    <*> (OA.switch $
            OA.long    "force" <>
            OA.help    "Force ANSI terminal" <>
            OA.hidden)
    <*> (OA.switch $
            OA.long    "dump" <>
            OA.short   'd' <>
            OA.help    "Just dump all slides and exit" <>
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
    options   <- OA.execParser parserInfo
    errOrPres <- readPresentation (oFilePath options)
    pres      <- either (errorAndExit . return) return errOrPres

    unless (oForce options) assertAnsiFeatures

    if oDump options
        then dumpPresentation pres
        else IO.hSetBuffering IO.stdin IO.NoBuffering >> loop pres
  where
    loop pres0 = do
        displayPresentation pres0
        c      <- readPresentationCommand
        update <- updatePresentation c pres0
        case update of
            ExitedPresentation        -> return ()
            UpdatedPresentation pres1 -> loop pres1
            ErroredPresentation err   -> errorAndExit [err]
