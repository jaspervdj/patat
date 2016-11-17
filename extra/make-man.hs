-- | This script generates a man page for patat.
import           Data.Char   (isSpace, toLower)
import           Data.List   (isPrefixOf)
import           Data.Time   (defaultTimeLocale, formatTime, getCurrentTime)
import qualified Text.Pandoc as Pandoc

getVersion :: IO String
getVersion =
    dropWhile isSpace . drop 1 . dropWhile (/= ':') . head .
    filter (\l -> "version:" `isPrefixOf` map toLower l) .
    map (dropWhile isSpace) . lines <$> readFile "patat.cabal"

main :: IO ()
main = do
    Right pandoc   <- Pandoc.readMarkdown Pandoc.def <$> readFile "README.md"
    Right template <- Pandoc.getDefaultTemplate Nothing "man"

    date    <- formatTime defaultTimeLocale "%B %d, %Y" <$> getCurrentTime
    version <- getVersion

    let writerOptions = Pandoc.def
            { Pandoc.writerStandalone = True
            , Pandoc.writerTemplate   = template
            , Pandoc.writerVariables  =
                [ ("author",  "Jasper Van der Jeugt")
                , ("title",   "patat manual")
                , ("date",    date)
                , ("footer",  "patat v" ++ version)
                , ("section", "1")
                ]
            }

    writeFile "patat.man" $ Pandoc.writeMan writerOptions pandoc
