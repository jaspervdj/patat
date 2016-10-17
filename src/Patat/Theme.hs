--------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Patat.Theme
    ( Theme (..)
    , defaultTheme

    , Style (..)

    , SyntaxHighlighting (..)
    , defaultSyntaxHighlighting
    , syntaxHighlight
    ) where


--------------------------------------------------------------------------------
import           Control.Monad          (forM_, mplus)
import qualified Data.Aeson             as A
import qualified Data.Aeson.TH.Extended as A
import           Data.Char              (toLower, toUpper)
import           Data.List              (intercalate, isSuffixOf)
import qualified Data.Map               as M
import           Data.Maybe             (mapMaybe, maybeToList)
import           Data.Monoid            (Monoid (..), (<>))
import qualified Data.Text              as T
import qualified System.Console.ANSI    as Ansi
import qualified Text.Highlighting.Kate as Kate
import           Text.Read              (readMaybe)
import           Prelude


--------------------------------------------------------------------------------
data Theme = Theme
    { themeBorders            :: !(Maybe Style)
    , themeHeader             :: !(Maybe Style)
    , themeCodeBlock          :: !(Maybe Style)
    , themeBulletList         :: !(Maybe Style)
    , themeBulletListMarkers  :: !(Maybe T.Text)
    , themeOrderedList        :: !(Maybe Style)
    , themeBlockQuote         :: !(Maybe Style)
    , themeDefinitionTerm     :: !(Maybe Style)
    , themeDefinitionList     :: !(Maybe Style)
    , themeTableHeader        :: !(Maybe Style)
    , themeTableSeparator     :: !(Maybe Style)
    , themeEmph               :: !(Maybe Style)
    , themeStrong             :: !(Maybe Style)
    , themeCode               :: !(Maybe Style)
    , themeLinkText           :: !(Maybe Style)
    , themeLinkTarget         :: !(Maybe Style)
    , themeStrikeout          :: !(Maybe Style)
    , themeQuoted             :: !(Maybe Style)
    , themeMath               :: !(Maybe Style)
    , themeImageText          :: !(Maybe Style)
    , themeImageTarget        :: !(Maybe Style)
    , themeSyntaxHighlighting :: !(Maybe SyntaxHighlighting)
    } deriving (Show)


--------------------------------------------------------------------------------
instance Monoid Theme where
    mempty = Theme
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Nothing Nothing Nothing Nothing

    mappend l r = Theme
        { themeBorders            = mplusOn   themeBorders
        , themeHeader             = mplusOn   themeHeader
        , themeCodeBlock          = mplusOn   themeCodeBlock
        , themeBulletList         = mplusOn   themeBulletList
        , themeBulletListMarkers  = mplusOn   themeBulletListMarkers
        , themeOrderedList        = mplusOn   themeOrderedList
        , themeBlockQuote         = mplusOn   themeBlockQuote
        , themeDefinitionTerm     = mplusOn   themeDefinitionTerm
        , themeDefinitionList     = mplusOn   themeDefinitionList
        , themeTableHeader        = mplusOn   themeTableHeader
        , themeTableSeparator     = mplusOn   themeTableSeparator
        , themeEmph               = mplusOn   themeEmph
        , themeStrong             = mplusOn   themeStrong
        , themeCode               = mplusOn   themeCode
        , themeLinkText           = mplusOn   themeLinkText
        , themeLinkTarget         = mplusOn   themeLinkTarget
        , themeStrikeout          = mplusOn   themeStrikeout
        , themeQuoted             = mplusOn   themeQuoted
        , themeMath               = mplusOn   themeMath
        , themeImageText          = mplusOn   themeImageText
        , themeImageTarget        = mplusOn   themeImageTarget
        , themeSyntaxHighlighting = mappendOn themeSyntaxHighlighting
        }
      where
        mplusOn   f = f l `mplus`   f r
        mappendOn f = f l `mappend` f r


--------------------------------------------------------------------------------
defaultTheme :: Theme
defaultTheme = Theme
    { themeBorders            = dull Ansi.Yellow
    , themeHeader             = dull Ansi.Blue
    , themeCodeBlock          = dull Ansi.White <> ondull Ansi.Black
    , themeBulletList         = dull Ansi.Magenta
    , themeBulletListMarkers  = Just "-*"
    , themeOrderedList        = dull Ansi.Magenta
    , themeBlockQuote         = dull Ansi.Green
    , themeDefinitionTerm     = dull Ansi.Blue
    , themeDefinitionList     = dull Ansi.Magenta
    , themeTableHeader        = dull Ansi.Blue
    , themeTableSeparator     = dull Ansi.Magenta
    , themeEmph               = dull Ansi.Green
    , themeStrong             = dull Ansi.Red <> bold
    , themeCode               = dull Ansi.White <> ondull Ansi.Black
    , themeLinkText           = dull Ansi.Green
    , themeLinkTarget         = dull Ansi.Cyan <> underline
    , themeStrikeout          = ondull Ansi.Red
    , themeQuoted             = dull Ansi.Green
    , themeMath               = dull Ansi.Green
    , themeImageText          = dull Ansi.Green
    , themeImageTarget        = dull Ansi.Cyan <> underline
    , themeSyntaxHighlighting = Just defaultSyntaxHighlighting
    }
  where
    dull   c  = Just $ Style [Ansi.SetColor Ansi.Foreground Ansi.Dull c]
    ondull c  = Just $ Style [Ansi.SetColor Ansi.Background Ansi.Dull c]
    bold      = Just $ Style [Ansi.SetConsoleIntensity Ansi.BoldIntensity]
    underline = Just $ Style [Ansi.SetUnderlining Ansi.SingleUnderline]


--------------------------------------------------------------------------------
newtype Style = Style {unStyle :: [Ansi.SGR]}
    deriving (Monoid, Show)


--------------------------------------------------------------------------------
instance A.ToJSON Style where
    toJSON = A.toJSON . mapMaybe nameForSGR . unStyle


--------------------------------------------------------------------------------
instance A.FromJSON Style where
    parseJSON val = do
        names <- A.parseJSON val
        sgrs  <- mapM toSgr names
        return $! Style sgrs
      where
        toSgr name = case M.lookup name sgrsByName of
            Just sgr -> return sgr
            Nothing  -> fail $!
                "Unknown style: " ++ show name ++ ". Known styles are: " ++
                intercalate ", " (map show $ M.keys sgrsByName)


--------------------------------------------------------------------------------
nameForSGR :: Ansi.SGR -> Maybe String
nameForSGR (Ansi.SetColor layer intensity color) = Just $
    (\str -> case layer of
        Ansi.Foreground -> str
        Ansi.Background -> "on" ++ capitalize str) $
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

nameForSGR (Ansi.SetUnderlining Ansi.SingleUnderline) = Just "underline"

nameForSGR (Ansi.SetConsoleIntensity Ansi.BoldIntensity) = Just "bold"

nameForSGR _ = Nothing


--------------------------------------------------------------------------------
sgrsByName :: M.Map String Ansi.SGR
sgrsByName = M.fromList
    [ (name, sgr)
    | sgr  <- knownSgrs
    , name <- maybeToList (nameForSGR sgr)
    ]
  where
    -- | It doesn't really matter if we generate "too much" SGRs here since
    -- 'nameForSGR' will only pick the ones we support.
    knownSgrs =
        [ Ansi.SetColor l i c
        | l <- [minBound .. maxBound]
        , i <- [minBound .. maxBound]
        , c <- [minBound .. maxBound]
        ] ++
        [Ansi.SetUnderlining      u | u <- [minBound .. maxBound]] ++
        [Ansi.SetConsoleIntensity c | c <- [minBound .. maxBound]]


--------------------------------------------------------------------------------
newtype SyntaxHighlighting = SyntaxHighlighting
    { unSyntaxHighlighting :: M.Map String Style
    } deriving (Monoid, Show, A.ToJSON)


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
    [ (Kate.KeywordTok,        dull Ansi.Yellow)
    , (Kate.ControlFlowTok,    dull Ansi.Yellow)

    , (Kate.DataTypeTok,       dull Ansi.Green)

    , (Kate.DecValTok,         dull Ansi.Red)
    , (Kate.BaseNTok,          dull Ansi.Red)
    , (Kate.FloatTok,          dull Ansi.Red)
    , (Kate.ConstantTok,       dull Ansi.Red)
    , (Kate.CharTok,           dull Ansi.Red)
    , (Kate.SpecialCharTok,    dull Ansi.Red)
    , (Kate.StringTok,         dull Ansi.Red)
    , (Kate.VerbatimStringTok, dull Ansi.Red)
    , (Kate.SpecialStringTok,  dull Ansi.Red)

    , (Kate.CommentTok,        dull Ansi.Blue)
    , (Kate.DocumentationTok,  dull Ansi.Blue)
    , (Kate.AnnotationTok,     dull Ansi.Blue)
    , (Kate.CommentVarTok,     dull Ansi.Blue)

    , (Kate.ImportTok,         dull Ansi.Cyan)
    , (Kate.OperatorTok,       dull Ansi.Cyan)
    , (Kate.FunctionTok,       dull Ansi.Cyan)
    , (Kate.PreprocessorTok,   dull Ansi.Cyan)
    ]
  where
    dull c = Style [Ansi.SetColor Ansi.Foreground Ansi.Dull c]

    mkSyntaxHighlighting ls = SyntaxHighlighting $
        M.fromList [(nameForTokenType tt, s) | (tt, s) <- ls]


--------------------------------------------------------------------------------
nameForTokenType :: Kate.TokenType -> String
nameForTokenType =
    unCapitalize . dropTok . show
  where
    unCapitalize (x : xs) = toLower x : xs
    unCapitalize xs       = xs

    dropTok str
        | "Tok" `isSuffixOf` str = take (length str - 3) str
        | otherwise              = str


--------------------------------------------------------------------------------
nameToTokenType :: String -> Maybe Kate.TokenType
nameToTokenType = readMaybe . capitalize . (++ "Tok")


--------------------------------------------------------------------------------
capitalize :: String -> String
capitalize ""       = ""
capitalize (x : xs) = toUpper x : xs


--------------------------------------------------------------------------------
syntaxHighlight :: Theme -> Kate.TokenType -> Maybe Style
syntaxHighlight theme tokenType = do
    sh <- themeSyntaxHighlighting theme
    M.lookup (nameForTokenType tokenType) (unSyntaxHighlighting sh)


--------------------------------------------------------------------------------
$(A.deriveJSON A.dropPrefixOptions ''Theme)
