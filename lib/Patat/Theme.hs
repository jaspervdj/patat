--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Theme
    ( Style (..)
    , BlockAlign (..)
    , BlockTheme (..)
    , BlockThemes (..)
    , Theme (..)
    , defaultTheme
    , themeForHeader

    , SyntaxHighlighting (..)
    , defaultSyntaxHighlighting
    , syntaxHighlight
    ) where


--------------------------------------------------------------------------------
import           Control.Monad          (forM_, mplus)
import qualified Data.Aeson             as A
import qualified Data.Aeson.TH.Extended as A
import           Data.Char              (toLower, toUpper)
import           Data.Colour.SRGB       (RGB (..), sRGB24reads, toSRGB24)
import           Data.List              (intercalate, isPrefixOf, isSuffixOf, foldl')
import qualified Data.Map               as M
import           Data.Maybe             (mapMaybe, maybeToList)
import qualified Data.Text              as T
import           Numeric                (showHex)
import           Prelude
import qualified Skylighting            as Skylighting
import qualified System.Console.ANSI    as Ansi
import           Text.Read              (readMaybe)


--------------------------------------------------------------------------------
newtype Style = Style {unStyle :: [Ansi.SGR]}
    deriving (Eq, Monoid, Semigroup, Show)


--------------------------------------------------------------------------------
instance A.ToJSON Style where
    toJSON = A.toJSON . mapMaybe sgrToString . unStyle


--------------------------------------------------------------------------------
instance A.FromJSON Style where
    parseJSON val = do
        names <- A.parseJSON val
        sgrs  <- mapM toSgr names
        return $! Style sgrs
      where
        toSgr name = case stringToSgr name of
            Just sgr -> return sgr
            Nothing  -> fail $!
                "Unknown style: " ++ show name ++ ". Known styles are: " ++
                intercalate ", " (map show $ M.keys namedSgrs) ++
                ", or \"rgb#RrGgBb\" and \"onRgb#RrGgBb\" where 'Rr', " ++
                "'Gg' and 'Bb' are hexadecimal bytes (e.g. \"rgb#f08000\")."


--------------------------------------------------------------------------------
stringToSgr :: String -> Maybe Ansi.SGR
stringToSgr s
    | "rgb#"   `isPrefixOf` s = rgbToSgr Ansi.Foreground $ drop 4 s
    | "onRgb#" `isPrefixOf` s = rgbToSgr Ansi.Background $ drop 6 s
    | otherwise               = M.lookup s namedSgrs


--------------------------------------------------------------------------------
rgbToSgr :: Ansi.ConsoleLayer -> String -> Maybe Ansi.SGR
rgbToSgr layer rgbHex =
    case sRGB24reads rgbHex of
        [(color, "")] -> Just $ Ansi.SetRGBColor layer color
        _             -> Nothing


--------------------------------------------------------------------------------
sgrToString :: Ansi.SGR -> Maybe String
sgrToString sgr = case sgr of
    Ansi.SetColor layer intensity color -> Just $ layerPrefix layer $
        (case intensity of
            Ansi.Dull  -> "dull"
            Ansi.Vivid -> "vivid") ++
        (case color of
            Ansi.Black   -> "Black"
            Ansi.Red     -> "Red"
            Ansi.Green   -> "Green"
            Ansi.Yellow  -> "Yellow"
            Ansi.Blue    -> "Blue"
            Ansi.Magenta -> "Magenta"
            Ansi.Cyan    -> "Cyan"
            Ansi.White   -> "White")

    Ansi.SetUnderlining Ansi.SingleUnderline -> Just "underline"

    Ansi.SetConsoleIntensity Ansi.BoldIntensity -> Just "bold"

    Ansi.SetItalicized True -> Just "italic"

    Ansi.SetRGBColor layer color -> Just $ layerPrefix layer $
        "rgb#" ++ (toRGBHex $ toSRGB24 color)

    _ -> Nothing
  where
    toRGBHex (RGB r g b) = concat $ map toHexByte [r, g, b]
    toHexByte x = showHex2 x ""
    showHex2 x | x <= 0xf = ("0" ++) . showHex x
               | otherwise = showHex x

    layerPrefix layer str = case layer of
        Ansi.Foreground -> str
        Ansi.Background -> "on" ++ capitalize str
        Ansi.Underlining -> "underline" ++ capitalize str


--------------------------------------------------------------------------------
nameForTokenType :: Skylighting.TokenType -> String
nameForTokenType =
    unCapitalize . dropTok . show
  where
    unCapitalize (x : xs) = toLower x : xs
    unCapitalize xs       = xs

    dropTok :: String -> String
    dropTok str
        | "Tok" `isSuffixOf` str = take (length str - 3) str
        | otherwise              = str


--------------------------------------------------------------------------------
nameToTokenType :: String -> Maybe Skylighting.TokenType
nameToTokenType = readMaybe . capitalize . (++ "Tok")


--------------------------------------------------------------------------------
capitalize :: String -> String
capitalize ""       = ""
capitalize (x : xs) = toUpper x : xs


--------------------------------------------------------------------------------
namedSgrs :: M.Map String Ansi.SGR
namedSgrs = M.fromList
    [ (name, sgr)
    | sgr  <- knownSgrs
    , name <- maybeToList (sgrToString sgr)
    ]
  where
    -- It doesn't really matter if we generate "too much" SGRs here since
    -- 'sgrToString' will only pick the ones we support.
    knownSgrs =
        [ Ansi.SetColor l i c
        | l <- [minBound .. maxBound]
        , i <- [minBound .. maxBound]
        , c <- [minBound .. maxBound]
        ] ++
        [Ansi.SetUnderlining      u | u <- [minBound .. maxBound]] ++
        [Ansi.SetConsoleIntensity c | c <- [minBound .. maxBound]] ++
        [Ansi.SetItalicized       i | i <- [minBound .. maxBound]]


--------------------------------------------------------------------------------
data BlockAlign = LeftBlockAlign | CenterBlockAlign
    deriving (Eq, Show)


--------------------------------------------------------------------------------
instance A.ToJSON BlockAlign where
    toJSON LeftBlockAlign   = "left"
    toJSON CenterBlockAlign = "center"


--------------------------------------------------------------------------------
instance A.FromJSON BlockAlign where
    parseJSON = A.withText "FromJSON BlockAlign" $ \txt -> case txt of
        "left"   -> pure LeftBlockAlign
        "center" -> pure CenterBlockAlign
        _        -> fail $ "Unknown align: " ++ show txt


--------------------------------------------------------------------------------
data BlockTheme = BlockTheme
    { btStyle     :: !(Maybe Style)
    , btPrefix    :: !(Maybe T.Text)
    , btUnderline :: !(Maybe T.Text)
    , btAlign     :: !(Maybe BlockAlign)
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
$(A.deriveJSON A.dropPrefixOptions ''BlockTheme)


--------------------------------------------------------------------------------
instance Semigroup BlockTheme where
    l <> r = BlockTheme
        { btStyle     = btStyle     l `mplus` btStyle     r
        , btPrefix    = btPrefix    l `mplus` btPrefix    r
        , btUnderline = btUnderline l `mplus` btUnderline r
        , btAlign     = btAlign     l `mplus` btAlign     r
        }


--------------------------------------------------------------------------------
newtype BlockThemes = BlockThemes (M.Map T.Text BlockTheme)
    deriving (Eq, Show, A.FromJSON, A.ToJSON)


--------------------------------------------------------------------------------
instance Semigroup BlockThemes where
    BlockThemes l <> BlockThemes r = BlockThemes $ M.unionWith (<>) l r


--------------------------------------------------------------------------------
data Theme = Theme
    { themeBorders            :: !(Maybe Style)
    , themeHeader             :: !(Maybe Style)
    , themeHeaders            :: !(Maybe BlockThemes)
    , themeCodeBlock          :: !(Maybe Style)
    , themeBulletList         :: !(Maybe Style)
    , themeBulletListMarkers  :: !(Maybe T.Text)
    , themeOrderedList        :: !(Maybe Style)
    , themeBlockQuote         :: !(Maybe Style)
    , themeDefinitionTerm     :: !(Maybe Style)
    , themeDefinitionList     :: !(Maybe Style)
    , themeTableHeader        :: !(Maybe Style)
    , themeTableSeparator     :: !(Maybe Style)
    , themeLineBlock          :: !(Maybe Style)
    , themeEmph               :: !(Maybe Style)
    , themeStrong             :: !(Maybe Style)
    , themeUnderline          :: !(Maybe Style)
    , themeCode               :: !(Maybe Style)
    , themeLinkText           :: !(Maybe Style)
    , themeLinkTarget         :: !(Maybe Style)
    , themeLinkOSC8           :: !(Maybe Style)  -- Undocumented
    , themeStrikeout          :: !(Maybe Style)
    , themeQuoted             :: !(Maybe Style)
    , themeMath               :: !(Maybe Style)
    , themeImageText          :: !(Maybe Style)
    , themeImageTarget        :: !(Maybe Style)
    , themeSyntaxHighlighting :: !(Maybe SyntaxHighlighting)
    , themeClass              :: !(Maybe BlockThemes)
    } deriving (Eq, Show)


--------------------------------------------------------------------------------
instance Semigroup Theme where
    l <> r = Theme
        { themeBorders            = mplusOn   themeBorders
        , themeHeader             = mplusOn   themeHeader
        , themeHeaders            = mappendOn themeHeaders
        , themeCodeBlock          = mplusOn   themeCodeBlock
        , themeBulletList         = mplusOn   themeBulletList
        , themeBulletListMarkers  = mplusOn   themeBulletListMarkers
        , themeOrderedList        = mplusOn   themeOrderedList
        , themeBlockQuote         = mplusOn   themeBlockQuote
        , themeDefinitionTerm     = mplusOn   themeDefinitionTerm
        , themeDefinitionList     = mplusOn   themeDefinitionList
        , themeTableHeader        = mplusOn   themeTableHeader
        , themeTableSeparator     = mplusOn   themeTableSeparator
        , themeLineBlock          = mplusOn   themeLineBlock
        , themeEmph               = mplusOn   themeEmph
        , themeStrong             = mplusOn   themeStrong
        , themeUnderline          = mplusOn   themeUnderline
        , themeCode               = mplusOn   themeCode
        , themeLinkText           = mplusOn   themeLinkText
        , themeLinkTarget         = mplusOn   themeLinkTarget
        , themeLinkOSC8           = mplusOn   themeLinkOSC8
        , themeStrikeout          = mplusOn   themeStrikeout
        , themeQuoted             = mplusOn   themeQuoted
        , themeMath               = mplusOn   themeMath
        , themeImageText          = mplusOn   themeImageText
        , themeImageTarget        = mplusOn   themeImageTarget
        , themeSyntaxHighlighting = mappendOn themeSyntaxHighlighting
        , themeClass              = mappendOn themeClass
        }
      where
        mplusOn   f = f l `mplus`   f r
        mappendOn f = f l `mappend` f r


--------------------------------------------------------------------------------
instance Monoid Theme where
    mappend = (<>)
    mempty  = Theme
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

--------------------------------------------------------------------------------
defaultTheme :: Theme
defaultTheme = Theme
    { themeBorders            = dull Ansi.Yellow
    , themeHeader             = dull Ansi.Blue
    , themeHeaders            = Just $ BlockThemes $ M.fromList $ do
        n <- [1 .. 6]
        let prefix = T.replicate n "#" <> " "
            key    = T.pack $ "h" <> show n
        pure (key, BlockTheme Nothing (Just prefix) Nothing Nothing)
    , themeCodeBlock          = dull Ansi.White `mappend` ondull Ansi.Black
    , themeBulletList         = dull Ansi.Magenta
    , themeBulletListMarkers  = Just "-*"
    , themeOrderedList        = dull Ansi.Magenta
    , themeBlockQuote         = dull Ansi.Green
    , themeDefinitionTerm     = dull Ansi.Blue
    , themeDefinitionList     = dull Ansi.Magenta
    , themeTableHeader        = dull Ansi.Magenta `mappend` bold
    , themeTableSeparator     = dull Ansi.Magenta
    , themeLineBlock          = dull Ansi.Magenta
    , themeEmph               = dull Ansi.Green
    , themeStrong             = dull Ansi.Red `mappend` bold
    , themeUnderline          = dull Ansi.Red `mappend` underline
    , themeCode               = dull Ansi.White `mappend` ondull Ansi.Black
    , themeLinkText           = dull Ansi.Green
    , themeLinkTarget         = dull Ansi.Cyan `mappend` underline
    , themeLinkOSC8           = underline
    , themeStrikeout          = ondull Ansi.Red
    , themeQuoted             = dull Ansi.Green
    , themeMath               = dull Ansi.Green
    , themeImageText          = dull Ansi.Green
    , themeImageTarget        = dull Ansi.Cyan `mappend` underline
    , themeSyntaxHighlighting = Just defaultSyntaxHighlighting
    , themeClass              = Nothing
    }
  where
    dull   c  = Just $ Style [Ansi.SetColor Ansi.Foreground Ansi.Dull c]
    ondull c  = Just $ Style [Ansi.SetColor Ansi.Background Ansi.Dull c]
    bold      = Just $ Style [Ansi.SetConsoleIntensity Ansi.BoldIntensity]
    underline = Just $ Style [Ansi.SetUnderlining Ansi.SingleUnderline]


--------------------------------------------------------------------------------
themeForHeader :: Int -> [T.Text] -> Theme -> BlockTheme
themeForHeader n classes theme = foldl'
    (\acc clas -> maybe acc (<> acc) $ do
        BlockThemes m <- themeClass theme
        M.lookup clas m)
    (maybe def (<> def) $ do
        BlockThemes m <- themeHeaders theme
        M.lookup key m)
    classes
  where
    def = BlockTheme (themeHeader theme) Nothing Nothing Nothing
    key = T.pack $ "h" <> show n


--------------------------------------------------------------------------------
newtype SyntaxHighlighting = SyntaxHighlighting
    { unSyntaxHighlighting :: M.Map String Style
    } deriving (Eq, Monoid, Semigroup, Show, A.ToJSON)


--------------------------------------------------------------------------------
instance A.FromJSON SyntaxHighlighting where
    parseJSON val = do
        styleMap <- A.parseJSON val
        forM_ (M.keys styleMap) $ \k -> case nameToTokenType k of
            Just _  -> return ()
            Nothing -> fail $ "Unknown token type: " ++ show k
        return (SyntaxHighlighting styleMap)


--------------------------------------------------------------------------------
defaultSyntaxHighlighting :: SyntaxHighlighting
defaultSyntaxHighlighting = mkSyntaxHighlighting
    [ (Skylighting.KeywordTok,        dull Ansi.Yellow)
    , (Skylighting.ControlFlowTok,    dull Ansi.Yellow)

    , (Skylighting.DataTypeTok,       dull Ansi.Green)

    , (Skylighting.DecValTok,         dull Ansi.Red)
    , (Skylighting.BaseNTok,          dull Ansi.Red)
    , (Skylighting.FloatTok,          dull Ansi.Red)
    , (Skylighting.ConstantTok,       dull Ansi.Red)
    , (Skylighting.CharTok,           dull Ansi.Red)
    , (Skylighting.SpecialCharTok,    dull Ansi.Red)
    , (Skylighting.StringTok,         dull Ansi.Red)
    , (Skylighting.VerbatimStringTok, dull Ansi.Red)
    , (Skylighting.SpecialStringTok,  dull Ansi.Red)

    , (Skylighting.CommentTok,        dull Ansi.Blue)
    , (Skylighting.DocumentationTok,  dull Ansi.Blue)
    , (Skylighting.AnnotationTok,     dull Ansi.Blue)
    , (Skylighting.CommentVarTok,     dull Ansi.Blue)

    , (Skylighting.ImportTok,         dull Ansi.Cyan)
    , (Skylighting.OperatorTok,       dull Ansi.Cyan)
    , (Skylighting.FunctionTok,       dull Ansi.Cyan)
    , (Skylighting.PreprocessorTok,   dull Ansi.Cyan)
    ]
  where
    dull c = Style [Ansi.SetColor Ansi.Foreground Ansi.Dull c]

    mkSyntaxHighlighting ls = SyntaxHighlighting $
        M.fromList [(nameForTokenType tt, s) | (tt, s) <- ls]


--------------------------------------------------------------------------------
syntaxHighlight :: Theme -> Skylighting.TokenType -> Maybe Style
syntaxHighlight theme tokenType = do
    sh <- themeSyntaxHighlighting theme
    M.lookup (nameForTokenType tokenType) (unSyntaxHighlighting sh)


--------------------------------------------------------------------------------
$(A.deriveJSON A.dropPrefixOptions ''Theme)
