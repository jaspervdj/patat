-- | This script generates a man page for patat.
{-# LANGUAGE CPP #-}
import           Control.Monad    (guard)
import           Data.Char        (isSpace, toLower)
import           Data.List        (isPrefixOf)
import           Data.Maybe       (isJust)
import qualified System.IO        as IO
import qualified System.Process   as Process
import qualified Text.Pandoc      as Pandoc
import qualified Text.Pandoc.Walk as Pandoc

getVersion :: IO String
getVersion =
    dropWhile isSpace . drop 1 . dropWhile (/= ':') . head .
    filter (\l -> "version:" `isPrefixOf` map toLower l) .
    map (dropWhile isSpace) . lines <$> readFile "patat.cabal"

removeLinks :: Pandoc.Pandoc -> Pandoc.Pandoc
removeLinks = Pandoc.walk $ \inline -> case inline of
    Pandoc.Link _ inlines _ -> Pandoc.Emph inlines
    _                       -> inline

promoteHeaders :: Pandoc.Pandoc -> Pandoc.Pandoc
promoteHeaders = Pandoc.walk $ \block -> case block of
    Pandoc.Header n attr inlines -> Pandoc.Header (max 1 $ n - 1) attr inlines
    _                            -> block

type Sections = [(Int, String, [Pandoc.Block])]

toSections :: Int -> [Pandoc.Block] -> Sections
toSections level = go
  where
    go []       = []
    go (h : xs) = case toSectionHeader h of
        Nothing         -> go xs
        Just (l, title) ->
            let (section, cont) = break (isJust . toSectionHeader) xs in
            (l, title, section) : go cont

    toSectionHeader :: Pandoc.Block -> Maybe (Int, String)
    toSectionHeader (Pandoc.Header l _ inlines) = do
        guard (l <= level)
        let doc = Pandoc.Pandoc Pandoc.nullMeta [Pandoc.Plain inlines]
        return (l, Pandoc.writeMarkdown Pandoc.def doc)
    toSectionHeader _ = Nothing

fromSections :: Sections -> [Pandoc.Block]
fromSections = concatMap $ \(level, title, blocks) ->
    Pandoc.Header level ("", [], []) [Pandoc.Str title] : blocks

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

withBlocks
    :: Pandoc.Pandoc -> ([Pandoc.Block] -> [Pandoc.Block]) -> Pandoc.Pandoc
withBlocks (Pandoc.Pandoc meta bs) f = Pandoc.Pandoc meta (f bs)

main :: IO ()
main = do
    Right pandoc0  <- Pandoc.readMarkdown Pandoc.def <$> readFile "README.md"
    Right template <- Pandoc.getDefaultTemplate Nothing "man"

    -- It's important for reproducible builds that we get the date from git.
    version <- getVersion
    date    <- Process.readProcess "git"
        ["log", "-1", "--format=%cd", "--date", "format:%B %d, %Y", "."] ""

    let writerOptions = Pandoc.def {
#if PANDOC_MINOR_VERSION >= 19
              Pandoc.writerTemplate   = Just template
#else
              Pandoc.writerStandalone = True
            , Pandoc.writerTemplate   = template
#endif
            , Pandoc.writerVariables  =
                [ ("author",  "Jasper Van der Jeugt")
                , ("title",   "patat manual")
                , ("date",    date)
                , ("footer",  "patat v" ++ version)
                , ("section", "1")
                ]
            }

    let pandoc1 = reorganizeSections $ removeLinks pandoc0

    putStr $ Pandoc.writeMan writerOptions pandoc1
    IO.hPutStrLn IO.stderr "Wrote man page."
