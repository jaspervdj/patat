--------------------------------------------------------------------------------
-- | Displaying code blocks, optionally with syntax highlighting.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Presentation.Display.CodeBlock
    ( prettyCodeBlock
    ) where


--------------------------------------------------------------------------------
import           Data.Char                        (toLower)
import           Data.List                        (find)
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as S
import           Patat.Presentation.Display.Table (themed)
import qualified Patat.PrettyPrint                as PP
import           Patat.Theme
import qualified Text.Highlighting.Kate           as Kate


--------------------------------------------------------------------------------
lower :: String -> String
lower = map toLower


--------------------------------------------------------------------------------
supportedLanguages :: S.Set String
supportedLanguages = S.fromList (map lower Kate.languages)


--------------------------------------------------------------------------------
highlight :: [String] -> String -> [Kate.SourceLine]
highlight classes rawCodeBlock =
    case find (\c -> lower c `S.member` supportedLanguages) classes of
        Nothing   -> zeroHighlight rawCodeBlock
        Just lang -> Kate.highlightAs lang rawCodeBlock


--------------------------------------------------------------------------------
-- | This does fake highlighting, everything becomes a normal token.  That makes
-- things a bit easier, since we only need to deal with one cases in the
-- renderer.
zeroHighlight :: String -> [Kate.SourceLine]
zeroHighlight str = [[(Kate.NormalTok, line)] | line <- lines str]


--------------------------------------------------------------------------------
prettyCodeBlock :: Theme -> [String] -> String -> PP.Doc
prettyCodeBlock theme@Theme {..} classes rawCodeBlock =
    PP.vcat (map blockified sourceLines) <>
    PP.hardline
  where
    sourceLines :: [Kate.SourceLine]
    sourceLines =
        [[]] ++ highlight classes rawCodeBlock ++ [[]]

    prettySourceLine :: Kate.SourceLine -> PP.Doc
    prettySourceLine = mconcat . map prettyToken

    prettyToken :: Kate.Token -> PP.Doc
    prettyToken (tokenType, str) =
        themed (syntaxHighlight theme tokenType) (PP.string str)

    sourceLineLength :: Kate.SourceLine -> Int
    sourceLineLength line = sum [length str | (_, str) <- line]

    blockWidth :: Int
    blockWidth = foldr max 0 (map sourceLineLength sourceLines)

    blockified :: Kate.SourceLine -> PP.Doc
    blockified line =
        let len    = sourceLineLength line
            indent = PP.NotTrimmable "   " in
        PP.indent indent indent $
        themed themeCodeBlock $
            " " <>
            prettySourceLine line <>
            PP.string (replicate (blockWidth - len) ' ') <> " "
