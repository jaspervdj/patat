-- | Read a presentation from disk.
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
module Patat.Presentation.Read
    ( readPresentation
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Except        (ExceptT (..), runExceptT,
                                              throwError)
import           Control.Monad.Trans         (liftIO)
import qualified Data.Aeson                  as A
import qualified Data.ByteString             as B
import           Data.Monoid                 ((<>))
import qualified Data.Set                    as Set
import qualified Data.Yaml                   as Yaml
import           Patat.Presentation.Internal
import           System.Directory            (doesFileExist, getHomeDirectory)
import           System.FilePath             (takeExtension, (</>))
import qualified Text.Pandoc.Error           as Pandoc
import qualified Text.Pandoc.Extended        as Pandoc


--------------------------------------------------------------------------------
readPresentation :: FilePath -> IO (Either String Presentation)
readPresentation filePath = runExceptT $ do
    src    <- liftIO $ readFile filePath
    reader <- case readExtension ext of
        Nothing -> throwError $ "Unknown file extension: " ++ show ext
        Just x  -> return x
    doc@(Pandoc.Pandoc meta _) <- case reader src of
        Left  e -> throwError $ "Could not parse document: " ++ show e
        Right x -> return x

    homeSettings <- ExceptT readHomeSettings
    metaSettings <- ExceptT $ return $ readMetaSettings meta
    let settings = metaSettings <> homeSettings <> defaultPresentationSettings

    ExceptT $ return $ pandocToPresentation filePath settings doc
  where
    ext = takeExtension filePath


--------------------------------------------------------------------------------
readExtension
    :: String -> Maybe (String -> Either Pandoc.PandocError Pandoc.Pandoc)
readExtension fileExt = case fileExt of
    ".md"  -> Just $ Pandoc.readMarkdown Pandoc.def
    ".lhs" -> Just $ Pandoc.readMarkdown lhsOpts
    ""     -> Just $ Pandoc.readMarkdown Pandoc.def
    ".org" -> Just $ Pandoc.readOrg Pandoc.def
    _      -> Nothing

  where
    lhsOpts = Pandoc.def
        { Pandoc.readerExtensions = Set.insert Pandoc.Ext_literate_haskell
            (Pandoc.readerExtensions Pandoc.def)
        }


--------------------------------------------------------------------------------
pandocToPresentation
    :: FilePath -> PresentationSettings -> Pandoc.Pandoc
    -> Either String Presentation
pandocToPresentation pFilePath pSettings pandoc@(Pandoc.Pandoc meta _) = do
    let !pTitle       = Pandoc.docTitle meta
        !pSlides      = pandocToSlides pandoc
        !pActiveSlide = 0
        !pAuthor      = concat (Pandoc.docAuthors meta)
    return Presentation {..}


--------------------------------------------------------------------------------
-- | Read settings from the metadata block in the Pandoc document.
readMetaSettings :: Pandoc.Meta -> Either String PresentationSettings
readMetaSettings meta = case Pandoc.lookupMeta "patat" meta of
    Nothing  -> return mempty
    Just val -> resultToEither $! A.fromJSON $! Pandoc.metaToJson val
  where
    resultToEither :: A.Result a -> Either String a
    resultToEither (A.Success x) = Right x
    resultToEither (A.Error   e) = Left $!
        "Error parsing patat settings from metadata: " ++ e


--------------------------------------------------------------------------------
-- | Read settings from "$HOME/.patat.yaml".
readHomeSettings :: IO (Either String PresentationSettings)
readHomeSettings = do
    home <- getHomeDirectory
    let path = home </> ".patat.yaml"
    exists <- doesFileExist path
    if not exists
        then return (Right mempty)
        else do
            contents <- B.readFile path
            return $! Yaml.decodeEither contents


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
