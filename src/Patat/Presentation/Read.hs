-- | Read a presentation from disk.
{-# LANGUAGE RecordWildCards #-}
module Patat.Presentation.Read
    ( readPresentation
    ) where


--------------------------------------------------------------------------------
import           Patat.Presentation.Internal
import           System.Environment             (getArgs)
import qualified Text.Pandoc                    as Pandoc


--------------------------------------------------------------------------------
readPresentation :: FilePath -> IO (Either String Presentation)
readPresentation filePath = do
    src <- readFile filePath
    return $ do
        doc <- case Pandoc.readMarkdown Pandoc.def src of
            Left err -> Left $ "Pandoc parsing error: " ++ show err
            Right x  -> Right x

        pandocToPresentation filePath doc


--------------------------------------------------------------------------------
pandocToPresentation :: FilePath -> Pandoc.Pandoc -> Either String Presentation
pandocToPresentation pFilePath pandoc@(Pandoc.Pandoc meta _) = do
    let pTitle       = Pandoc.docTitle meta
        pSlides      = pandocToSlides pandoc
        pActiveSlide = 0
        pAuthor      = concat (Pandoc.docAuthors meta)
    return Presentation {..}


--------------------------------------------------------------------------------
-- | Split a pandoc document into slides.  If the document contains horizonal
-- rules, we use those as slide delimiters.  If there are no horizontal rules,
-- we split using h1 headers.
pandocToSlides :: Pandoc.Pandoc -> [Slide]
pandocToSlides (Pandoc.Pandoc _meta blocks0)
    | any (== Pandoc.HorizontalRule) blocks0 = splitAtRules blocks0
    | otherwise                              = splitAtH1s   blocks0
  where
    splitAtRules blocks = case break (== Pandoc.HorizontalRule) blocks of
        (xs, [])           -> [Slide xs]
        (xs, (_rule : ys)) -> Slide xs : splitAtRules ys

    splitAtH1s []       = []
    splitAtH1s (b : bs) = case break isH1 bs of
        (xs, [])       -> [Slide (b : xs)]
        (xs, (y : ys)) -> Slide (b : xs) : splitAtH1s (y : ys)

    isH1 (Pandoc.Header i _ _) = i == 1
    isH1 _                     = False
