{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Eval.Internal
    ( EvalBlocks
    , EvalBlock (..)
    , renderEvalBlock
    ) where


--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async       as Async
import qualified Data.HashMap.Strict            as HMS
import qualified Data.Text                      as T
import           Patat.Presentation.Instruction
import           Patat.Presentation.Settings
import           Patat.Presentation.Syntax
import qualified Text.Pandoc                    as Pandoc


--------------------------------------------------------------------------------
type EvalBlocks = HMS.HashMap Var EvalBlock


--------------------------------------------------------------------------------
-- | Block that needs to be evaluated.
data EvalBlock = EvalBlock
    { ebSettings :: !EvalSettings
    , ebAttr     :: !Pandoc.Attr
    , ebInput    :: !T.Text
    , ebAsync    :: !(Maybe (Async.Async ()))
    }


--------------------------------------------------------------------------------
renderEvalBlock :: EvalBlock -> T.Text -> [Block]
renderEvalBlock EvalBlock {..} out = case evalContainer ebSettings of
    EvalContainerCode   -> [CodeBlock ebAttr out]
    EvalContainerNone   -> [RawBlock fmt out]
    EvalContainerInline -> [Plain [RawInline fmt out]]
  where
    fmt = "eval"
