-- | Read a presentation from disk.
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Presentation.Read
    ( readPresentation
    ) where


--------------------------------------------------------------------------------
import           Control.Monad.Except        (ExceptT (..), runExceptT,
                                              throwError)
import           Control.Monad.Trans         (liftIO)
import qualified Data.Aeson                  as A
import qualified Data.ByteString             as B
import qualified Data.HashMap.Strict         as HMS
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (mempty, (<>))
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Yaml                   as Yaml
import           Patat.Presentation.Fragment
import           Patat.Presentation.Internal
import           Prelude
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
    doc    <- case reader src of
        Left  e -> throwError $ "Could not parse document: " ++ show e
        Right x -> return x

    homeSettings <- ExceptT readHomeSettings
    metaSettings <- ExceptT $ return $ readMetaSettings src
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
    let !pTitle          = Pandoc.docTitle meta
        !pSlides         = pandocToSlides pSettings pandoc
        !pActiveFragment = (0, 0)
        !pAuthor         = concat (Pandoc.docAuthors meta)
    return Presentation {..}


--------------------------------------------------------------------------------
-- | This re-parses the pandoc metadata block using the YAML library.  This
-- avoids the problems caused by pandoc involving rendering Markdown.  This
-- should only be used for settings though, not things like title / authors
-- since those /can/ contain markdown.
parseMetadataBlock :: String -> Maybe A.Value
parseMetadataBlock src = do
    block <- mbBlock
    Yaml.decode $! T.encodeUtf8 $! T.pack block
  where
    mbBlock = case lines src of
        ("---" : ls) -> case break (`elem` ["---", "..."]) ls of
            (_,     [])      -> Nothing
            (block, (_ : _)) -> Just (unlines block)
        _            -> Nothing


--------------------------------------------------------------------------------
-- | Read settings from the metadata block in the Pandoc document.
readMetaSettings :: String -> Either String PresentationSettings
readMetaSettings src = fromMaybe (Right mempty) $ do
    A.Object obj <- parseMetadataBlock src
    val          <- HMS.lookup "patat" obj
    return $! resultToEither $! A.fromJSON val
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
pandocToSlides :: PresentationSettings -> Pandoc.Pandoc -> [Slide]
pandocToSlides settings pandoc =
    let blockss = splitSlides pandoc in
    map (Slide . map Fragment . (fragmentBlocks fragmentSettings)) blockss
  where
    fragmentSettings = FragmentSettings
        { fsIncrementalLists = fromMaybe False (psIncrementalLists settings)
        }


--------------------------------------------------------------------------------
-- | Split a pandoc document into slides.  If the document contains horizonal
-- rules, we use those as slide delimiters.  If there are no horizontal rules,
-- we split using h1 headers.
splitSlides :: Pandoc.Pandoc -> [[Pandoc.Block]]
splitSlides (Pandoc.Pandoc _meta blocks0)
    | any (== Pandoc.HorizontalRule) blocks0 = splitAtRules blocks0
    | otherwise                              = splitAtH1s   blocks0
  where
    splitAtRules blocks = case break (== Pandoc.HorizontalRule) blocks of
        (xs, [])           -> [xs]
        (xs, (_rule : ys)) -> xs : splitAtRules ys

    splitAtH1s []       = []
    splitAtH1s (b : bs) = case break isH1 bs of
        (xs, [])       -> [(b : xs)]
        (xs, (y : ys)) -> (b : xs) : splitAtH1s (y : ys)

    isH1 (Pandoc.Header i _ _) = i == 1
    isH1 _                     = False
