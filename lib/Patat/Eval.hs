--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Patat.Eval
    ( eval
    ) where


--------------------------------------------------------------------------------
import           Patat.Presentation.Internal
import qualified System.Process              as Process
import           Text.Pandoc                 (Pandoc)
import qualified Text.Pandoc.Definition      as Pandoc
import qualified Text.Pandoc.Walk            as Pandoc


--------------------------------------------------------------------------------
eval :: Pandoc -> IO Pandoc
eval = Pandoc.walkM (fmap concat . mapM evalBlock)


--------------------------------------------------------------------------------
evalBlock :: Pandoc.Block -> IO [Pandoc.Block]
evalBlock block@(Pandoc.CodeBlock (_, classes, _) txt) | "eval" `elem` classes =
    pure [block]
evalBlock block =
    pure [block]
