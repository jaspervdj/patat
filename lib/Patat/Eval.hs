--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Eval
    ( eval
    ) where


--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async       as Async
import           Control.Exception              (finally)
import qualified Data.HashMap.Strict            as HMS
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Patat.Presentation.Instruction
import           Patat.Presentation.Internal
import           System.Exit                    (ExitCode (..))
import qualified System.IO                      as IO
import Data.Maybe (maybeToList)
import           System.IO.Unsafe               (unsafeInterleaveIO)
import qualified System.Process                 as Process
import qualified Text.Pandoc.Definition         as Pandoc


--------------------------------------------------------------------------------
eval :: Presentation -> IO Presentation
eval presentation = case psEval (pSettings presentation) of
    Nothing -> pure presentation
    Just settings -> do
        slides <- traverse (evalSlide settings) (pSlides presentation)
        pure presentation {pSlides = slides}


--------------------------------------------------------------------------------
lookupSettings :: [T.Text] -> EvalSettingsMap -> [EvalSettings]
lookupSettings classes settings = do
    c <- classes
    maybeToList $ HMS.lookup c settings


--------------------------------------------------------------------------------
evalSlide :: EvalSettingsMap -> Slide -> IO Slide
evalSlide settings slide = case slide of
    TitleSlide _ _ -> pure slide
    ContentSlide instrs -> ContentSlide . fromList . concat <$>
        traverse (evalInstruction settings) (toList instrs)


--------------------------------------------------------------------------------
evalInstruction
    :: EvalSettingsMap -> Instruction Pandoc.Block
    -> IO [Instruction Pandoc.Block]
evalInstruction settings instr = case instr of
    Pause         -> pure [Pause]
    ModifyLast i  -> map ModifyLast <$> evalInstruction settings i
    Append []     -> pure [Append []]
    Append blocks -> concat <$> traverse (evalBlock settings) blocks
    Delete        -> pure [Delete]


--------------------------------------------------------------------------------
evalBlock :: EvalSettingsMap -> Pandoc.Block -> IO [Instruction Pandoc.Block]
evalBlock settings orig@(Pandoc.CodeBlock attr@(_, classes, _) txt)
    | [s@EvalSettings {..}] <- lookupSettings classes settings =
        unsafeInterleaveIO $ do
        EvalResult {..} <- evalCode s txt
        let out = case erExitCode of
                ExitSuccess -> erStdout
                ExitFailure i ->
                    evalCommand <> ": exit code " <> T.pack (show i) <> "\n" <>
                    erStderr
        pure $ case (evalFragment, evalReplace) of
            (False, True) -> [Append [Pandoc.CodeBlock attr out]]
            (False, False) -> [Append [orig, Pandoc.CodeBlock attr out]]
            (True, True) ->
                [ Append [orig], Pause
                , Delete, Append [Pandoc.CodeBlock attr out]
                ]
            (True, False) ->
                [Append [orig], Pause, Append [Pandoc.CodeBlock attr out]]
    | _ : _ : _ <- lookupSettings classes settings =
        let msg = "patat eval matched multiple settings for " <>
                T.intercalate "," classes in
        pure [Append [Pandoc.CodeBlock attr msg]]
evalBlock _ block =
    pure [Append [block]]


--------------------------------------------------------------------------------
data EvalResult = EvalResult
    { erExitCode :: !ExitCode
    , erStdout   :: !T.Text
    , erStderr   :: !T.Text
    } deriving (Show)


--------------------------------------------------------------------------------
evalCode :: EvalSettings -> T.Text -> IO EvalResult
evalCode EvalSettings {..} input = do
    let proc = (Process.shell $ T.unpack evalCommand)
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

    (Just hIn, Just hOut, Just hErr, hProc) <- Process.createProcess proc

    Async.withAsync (T.hPutStr hIn input `finally` IO.hClose hIn) $ \_ ->
        Async.withAsync (T.hGetContents hOut) $ \outAsync ->
        Async.withAsync (T.hGetContents hErr) $ \errAsync ->
        Async.withAsync (Process.waitForProcess hProc) $ \exitCodeAsync -> do

        erExitCode <- Async.wait exitCodeAsync
        erStdout <- Async.wait outAsync
        erStderr <- Async.wait errAsync
        pure $ EvalResult {..}
