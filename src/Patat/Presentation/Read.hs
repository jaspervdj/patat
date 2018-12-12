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
import qualified Data.HashMap.Strict         as HMS
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 (mempty, (<>))
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.IO                as T
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
    -- We need to read the settings first.
    src          <- liftIO $ T.readFile filePath
    homeSettings <- ExceptT readHomeSettings
    metaSettings <- ExceptT $ return $ readMetaSettings src
    let settings = metaSettings <> homeSettings <> defaultPresentationSettings

    let pexts = fromMaybe defaultExtensionList (psPandocExtensions settings)
    reader <- case readExtension pexts ext of
        Nothing -> throwError $ "Unknown file extension: " ++ show ext
        Just x  -> return x
    doc    <- case reader src of
        Left  e -> throwError $ "Could not parse document: " ++ show e
        Right x -> return x

    ExceptT $ return $ pandocToPresentation filePath settings doc
  where
    ext = takeExtension filePath


--------------------------------------------------------------------------------
readExtension
    :: ExtensionList -> String
    -> Maybe (T.Text -> Either Pandoc.PandocError Pandoc.Pandoc)
readExtension (ExtensionList extensions) fileExt = case fileExt of
    ".md"  -> Just $ Pandoc.runPure . Pandoc.readMarkdown readerOpts
    ".lhs" -> Just $ Pandoc.runPure . Pandoc.readMarkdown lhsOpts
    ""     -> Just $ Pandoc.runPure . Pandoc.readMarkdown readerOpts
    ".org" -> Just $ Pandoc.runPure . Pandoc.readOrg      readerOpts
    _      -> Nothing

  where
    readerOpts = Pandoc.def
        { Pandoc.readerExtensions =
            extensions <> absolutelyRequiredExtensions
        }

    lhsOpts = readerOpts
        { Pandoc.readerExtensions =
            Pandoc.readerExtensions readerOpts <>
            Pandoc.extensionsFromList [Pandoc.Ext_literate_haskell]
        }

    absolutelyRequiredExtensions =
        Pandoc.extensionsFromList [Pandoc.Ext_yaml_metadata_block]


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
parseMetadataBlock :: T.Text -> Maybe A.Value
parseMetadataBlock src = do
    block <- T.encodeUtf8 <$> mbBlock
    either (const Nothing) Just (Yaml.decodeEither' block)
  where
    mbBlock :: Maybe T.Text
    mbBlock = case T.lines src of
        ("---" : ls) -> case break (`elem` ["---", "..."]) ls of
            (_,     [])      -> Nothing
            (block, (_ : _)) -> Just (T.unlines block)
        _            -> Nothing


--------------------------------------------------------------------------------
-- | Read settings from the metadata block in the Pandoc document.
readMetaSettings :: T.Text -> Either String PresentationSettings
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
            errOrPs <- Yaml.decodeFileEither path
            return $! case errOrPs of
                Left  err -> Left (show err)
                Right ps  -> Right ps


--------------------------------------------------------------------------------
pandocToSlides :: PresentationSettings -> Pandoc.Pandoc -> [Slide]
pandocToSlides settings pandoc =
    let slideLevel   = fromMaybe (detectSlideLevel pandoc) (psSlideLevel settings)
        unfragmented = splitSlides slideLevel pandoc
        fragmented   =
            [ case slide of
                TitleSlide   _          -> slide
                ContentSlide fragments0 ->
                    let blocks  = concatMap unFragment fragments0
                        blockss = fragmentBlocks fragmentSettings blocks in
                    ContentSlide (map Fragment blockss)
            | slide <- unfragmented
            ] in
    fragmented
  where
    fragmentSettings = FragmentSettings
        { fsIncrementalLists = fromMaybe False (psIncrementalLists settings)
        }


--------------------------------------------------------------------------------
-- | Find level of header that starts slides.  This is defined as the least
-- header that occurs before a non-header in the blocks.
detectSlideLevel :: Pandoc.Pandoc -> Int
detectSlideLevel (Pandoc.Pandoc _meta blocks0) =
    go 6 blocks0
  where
    go level (Pandoc.Header n _ _ : x : xs)
        | n < level && nonHeader x = go n xs
        | otherwise                = go level (x:xs)
    go level (_ : xs)              = go level xs
    go level []                    = level

    nonHeader (Pandoc.Header _ _ _) = False
    nonHeader _                     = True


--------------------------------------------------------------------------------
-- | Split a pandoc document into slides.  If the document contains horizonal
-- rules, we use those as slide delimiters.  If there are no horizontal rules,
-- we split using headers, determined by the slide level (see
-- 'detectSlideLevel').
splitSlides :: Int -> Pandoc.Pandoc -> [Slide]
splitSlides slideLevel (Pandoc.Pandoc _meta blocks0)
    | any (== Pandoc.HorizontalRule) blocks0 = splitAtRules   blocks0
    | otherwise                              = splitAtHeaders [] blocks0
  where
    mkContentSlide :: [Pandoc.Block] -> [Slide]
    mkContentSlide [] = []  -- Never create empty slides
    mkContentSlide bs = [ContentSlide [Fragment bs]]

    splitAtRules blocks = case break (== Pandoc.HorizontalRule) blocks of
        (xs, [])           -> mkContentSlide xs
        (xs, (_rule : ys)) -> mkContentSlide xs ++ splitAtRules ys

    splitAtHeaders acc [] =
        mkContentSlide (reverse acc)
    splitAtHeaders acc (b@(Pandoc.Header i _ _) : bs)
        | i > slideLevel  = splitAtHeaders (b : acc) bs
        | i == slideLevel =
            mkContentSlide (reverse acc) ++ splitAtHeaders [b] bs
        | otherwise       =
            mkContentSlide (reverse acc) ++ [TitleSlide b] ++ splitAtHeaders [] bs
    splitAtHeaders acc (b : bs) =
        splitAtHeaders (b : acc) bs
