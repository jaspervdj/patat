-- | This script generates a man page for patat.
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Control.Exception   (throw)
import           Control.Monad       (guard)
import           Control.Monad.Trans (liftIO)
import           Data.Char           (isSpace, toLower)
import           Data.List           (isPrefixOf)
import           Data.Maybe          (isJust)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified GHC.IO.Encoding     as Encoding
import           Prelude
import           System.Environment  (getEnv)
import qualified System.IO           as IO
import qualified Data.Time as Time
import qualified Text.Pandoc         as Pandoc

getVersion :: IO String
getVersion =
    dropWhile isSpace . drop 1 . dropWhile (/= ':') . head .
    filter (\l -> "version:" `isPrefixOf` map toLower l) .
    map (dropWhile isSpace) . lines <$> readFile "patat.cabal"

getPrettySourceDate :: IO String
getPrettySourceDate = do
    epoch <- getEnv "SOURCE_DATE_EPOCH"
    utc   <- Time.parseTimeM True locale "%s" epoch :: IO Time.UTCTime
    return $ Time.formatTime locale "%B %d, %Y" utc
  where
    locale = Time.defaultTimeLocale

type Sections = [(Int, T.Text, [Pandoc.Block])]

toSections :: Int -> [Pandoc.Block] -> Sections
toSections level = go
  where
    go []       = []
    go (h : xs) = case toSectionHeader h of
        Nothing         -> go xs
        Just (l, title) ->
            let (section, cont) = break (isJust . toSectionHeader) xs in
            (l, title, section) : go cont

    toSectionHeader :: Pandoc.Block -> Maybe (Int, T.Text)
    toSectionHeader (Pandoc.Header l _ inlines) = do
        guard (l <= level)
        let doc = Pandoc.Pandoc Pandoc.nullMeta [Pandoc.Plain inlines]
            txt = case Pandoc.runPure (Pandoc.writeMarkdown Pandoc.def doc) of
                    Left err -> throw err  -- Bad!
                    Right x  -> x
        return (l, txt)
    toSectionHeader _ = Nothing

fromSections :: Sections -> [Pandoc.Block]
fromSections = concatMap $ \(level, title, blocks) ->
    Pandoc.Header level ("", [], []) [Pandoc.Str $ T.unpack title] : blocks

reorganizeSections :: Pandoc.Pandoc -> Pandoc.Pandoc
reorganizeSections (Pandoc.Pandoc meta0 blocks0) =
    let sections0 = toSections 2 blocks0 in
    Pandoc.Pandoc meta0 $ fromSections $
        [ (1, "NAME", nameSection)
        ] ++
        [ (1, "SYNOPSIS", s)
        | (_, _, s) <- lookupSection "Running" sections0
        ] ++
        [ (1, "DESCRIPTION", [])
        ] ++
            [ (2, n, s)
            | (_, n, s) <- lookupSection "Controls" sections0
            ] ++
            [ (2, n, s)
            | (_, n, s) <- lookupSection "Input format" sections0
            ] ++
            [ (2, n, s)
            | (_, n, s) <- lookupSection "Configuration" sections0
            ] ++
        [ (1, "OPTIONS", s)
        | (_, _, s) <- lookupSection "Options" sections0
        ] ++
        [ (1, "SEE ALSO", seeAlsoSection)
        ]
  where
    nameSection    = mkPara "patat - Presentations Atop The ANSI Terminal"
    seeAlsoSection = mkPara "pandoc(1)"
    mkPara str     = [Pandoc.Para [Pandoc.Str str]]

    lookupSection name sections =
        [section | section@(_, n, _) <- sections, name == n]

main :: IO ()
main = Pandoc.runIOorExplode $ do
    liftIO $ Encoding.setLocaleEncoding Encoding.utf8

    let readerOptions = Pandoc.def
            { Pandoc.readerExtensions = Pandoc.pandocExtensions
            }

    source   <- liftIO $ T.readFile "README.md"
    pandoc0  <- Pandoc.readMarkdown readerOptions source
    template <- Pandoc.getDefaultTemplate "man"

    version <- liftIO getVersion
    date    <- liftIO getPrettySourceDate

    let writerOptions = Pandoc.def
            { Pandoc.writerTemplate   = Just template
            , Pandoc.writerVariables  =
                [ ("author",  "Jasper Van der Jeugt")
                , ("title",   "patat manual")
                , ("date",    date)
                , ("footer",  "patat v" ++ version)
                , ("section", "1")
                ]
            }

    let pandoc1 = reorganizeSections $ pandoc0
    txt <- Pandoc.writeMan writerOptions pandoc1
    liftIO $ do
        T.putStr txt
        IO.hPutStrLn IO.stderr "Wrote man page."
