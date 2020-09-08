--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Eval
    ( eval
    ) where


--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async       as Async
import           Control.Exception              (finally)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Patat.Presentation.Instruction
import           Patat.Presentation.Internal
import           System.Exit                    (ExitCode (..))
import qualified System.IO                      as IO
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
evalSlide :: EvalSettings -> Slide -> IO Slide
evalSlide settings slide = case slide of
    TitleSlide _ _ -> pure slide
    ContentSlide instrs -> ContentSlide . fromList . concat <$>
        traverse (evalInstruction settings) (toList instrs)


--------------------------------------------------------------------------------
evalInstruction
    :: EvalSettings -> Instruction Pandoc.Block -> IO [Instruction Pandoc.Block]
evalInstruction settings instr = case instr of
    Pause -> pure [Pause]
    ModifyLast i -> map ModifyLast <$> evalInstruction settings i
    Append blocks -> concat <$> traverse (evalBlock settings) blocks


--------------------------------------------------------------------------------
evalBlock :: EvalSettings -> Pandoc.Block -> IO [Instruction Pandoc.Block]
evalBlock s@EvalSettings {..} orig@(Pandoc.CodeBlock attr@(_, classes, _) txt)
    | "eval" `elem` classes = unsafeInterleaveIO $ do
        EvalResult {..} <- evalCode s txt
        let out = case erExitCode of
                ExitSuccess -> erStdout
                ExitFailure i ->
                    evalCommand <> ": exit code " <> T.pack (show i) <> "\n" <>
                    erStderr
        pure [Append [orig], Pause, Append [Pandoc.CodeBlock attr out]]
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
