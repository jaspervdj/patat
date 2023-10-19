--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Eval
    ( eval
    ) where


--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async       as Async
import           Control.Exception              (finally)
import           Control.Monad.Except           (ExceptT (..), runExceptT,
                                                 throwError)
import           Control.Monad.Trans            (liftIO)
import qualified Data.HashMap.Strict            as HMS
import           Data.Maybe                     (maybeToList)
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
eval :: Presentation -> IO (Either String Presentation)
eval pres = case psEval (pSettings pres) of
    Nothing -> pure $ Right pres
    Just settings -> runExceptT $ do
        let env = EvalEnv (pFilePath pres) (pFileExecutable pres) settings
        slides <- traverse (evalSlide env) (pSlides pres)
        pure $ pres {pSlides = slides}


--------------------------------------------------------------------------------
data EvalEnv = EvalEnv
    { eeFilePath       :: FilePath
    , eeFileExecutable :: IsExecutable
    , eeEvalSettings   :: EvalSettingsMap
    }


--------------------------------------------------------------------------------
lookupSettings :: EvalEnv -> [T.Text] -> [EvalSettings]
lookupSettings ee classes  = do
    c <- classes
    maybeToList $ HMS.lookup c (eeEvalSettings ee)


--------------------------------------------------------------------------------
evalSlide :: EvalEnv -> Slide -> ExceptT String IO Slide
evalSlide ee slide = case slideContent slide of
    TitleSlide _ _ -> pure slide
    ContentSlide instrs0 -> do
        instrs1 <- traverse (evalInstruction ee) (toList instrs0)
        pure slide {slideContent = ContentSlide . fromList $ concat instrs1}


--------------------------------------------------------------------------------
evalInstruction
    :: EvalEnv -> Instruction Pandoc.Block
    -> ExceptT String IO [Instruction Pandoc.Block]
evalInstruction ee instr = case instr of
    Pause         -> pure [Pause]
    ModifyLast i  -> map ModifyLast <$> evalInstruction ee i
    Append []     -> pure [Append []]
    Append blocks -> concat <$> traverse (evalBlock ee) blocks
    Delete        -> pure [Delete]


--------------------------------------------------------------------------------
evalBlock
    :: EvalEnv -> Pandoc.Block
    -> ExceptT String IO [Instruction Pandoc.Block]
evalBlock ee orig@(Pandoc.CodeBlock attr@(_, classes, _) txt)
    | _ : _ <- lookupSettings ee classes
    , eeFileExecutable ee == IsNotExecutable = throwError $
        "The file " <> eeFilePath ee <>
        " contains eval settings but is not executable. " <>
        "You can use `chmod +x " <> eeFilePath ee <>
        "` if you trust evaluating code in this file."
    | [s@EvalSettings {..}] <- lookupSettings ee classes = do
        out <- liftIO . unsafeInterleaveIO $ do
            EvalResult {..} <-  evalCode s txt
            pure $ case erExitCode of
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
    | _ : _ : _ <- lookupSettings ee classes = throwError $
        "patat eval matched multiple settings for " <>
        T.unpack (T.intercalate "," classes)
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
