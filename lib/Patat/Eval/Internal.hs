{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Eval.Internal
    ( EvalBlocks
    , EvalBlock (..)
    , renderEvalBlock
    ) where


--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async    as Async
import           Data.CaseInsensitive        (CI)
import qualified Data.HashMap.Strict         as HMS
import qualified Data.Text                   as T
import           Patat.Presentation.Settings
import           Patat.Presentation.Syntax


--------------------------------------------------------------------------------
type EvalBlocks = HMS.HashMap Var EvalBlock


--------------------------------------------------------------------------------
-- | Block that needs to be evaluated.
data EvalBlock = EvalBlock
    { ebSettings :: !EvalSettings
    , ebClasses  :: ![CI T.Text]
    , ebInput    :: !T.Text
    , ebAsync    :: !(Maybe (Async.Async ()))
    }


--------------------------------------------------------------------------------
renderEvalBlock :: EvalBlock -> T.Text -> [Block]
renderEvalBlock EvalBlock {..} out = case evalContainer ebSettings of
    EvalContainerCode   -> [CodeBlock classes out]
    EvalContainerNone   -> [RawBlock fmt out]
    EvalContainerInline -> [Plain [RawInline fmt out]]
  where
    fmt = "eval"

    -- The classes for the new code block are copied from the old one if
    -- unspecified, or the syntax specified in the eval settings.
    classes = maybe ebClasses pure $ evalSyntax ebSettings
