{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Eval.Internal
    ( EvalBlock (..)
    , EvalState (..)
    , Handle (..)
    , emptyHandle
    , startEval
    ) where


--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async       as Async
import           Control.Exception              (finally)
import           Control.Monad                  (when)
import qualified Data.HashMap.Strict            as HMS
import           Data.IORef                     (IORef, atomicModifyIORef',
                                                 writeIORef)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Patat.Presentation.Instruction
import           Patat.Presentation.Settings
import           System.Exit                    (ExitCode (..))
import qualified System.IO                      as IO
import qualified System.Process                 as Process


--------------------------------------------------------------------------------
-- | Block that needs to be evaluated.
data EvalBlock = EvalBlock
    { ebSettings :: EvalSettings
    , ebInput    :: T.Text
    , ebState    :: IORef EvalState
    }


--------------------------------------------------------------------------------
data EvalState
    = NotRunning
    | Starting
    | Running (Async.Async ())


--------------------------------------------------------------------------------
data Handle = Handle
    { hBlocks :: HMS.HashMap Var EvalBlock
    , hOutput :: HMS.HashMap Var (IORef T.Text)
    }


--------------------------------------------------------------------------------
emptyHandle :: Handle
emptyHandle = Handle HMS.empty HMS.empty


--------------------------------------------------------------------------------
startEval :: Handle -> Var -> IO () -> IO ()
startEval Handle {..} var notify = case HMS.lookup var hBlocks of
    Nothing -> pure ()
    Just EvalBlock {..} -> do
        let EvalSettings {..} = ebSettings
        needStart <- atomicModifyIORef' ebState $ \mbRunning ->
            case mbRunning of
                NotRunning -> (Starting, True)
                Starting   -> (Starting, False)
                Running r  -> (Running r, False)
        when needStart $ do
            let proc = (Process.shell $ T.unpack evalCommand)
                    { Process.std_in  = Process.CreatePipe
                    , Process.std_out = Process.CreatePipe
                    , Process.std_err = Process.CreatePipe
                    }
            (Just hIn, Just hOut, Just hErr, hProc) <- Process.createProcess proc
            async <- Async.async $
                Async.withAsync (T.hPutStr hIn ebInput `finally` IO.hClose hIn) $ \_ ->
                Async.withAsync (T.hGetContents hOut) $ \outAsync ->
                Async.withAsync (T.hGetContents hErr) $ \errAsync ->
                Async.withAsync (Process.waitForProcess hProc) $ \exitCodeAsync -> do
                erExitCode <- Async.wait exitCodeAsync
                erStdout <- Async.wait outAsync
                erStderr <- Async.wait errAsync
                let out = case erExitCode of
                        ExitSuccess -> erStdout
                        ExitFailure i ->
                            evalCommand <> ": exit code " <> T.pack (show i) <> "\n" <>
                            erStderr
                writeOutput out
            writeIORef ebState $ Running async
  where
    writeOutput out = case HMS.lookup var hOutput of
        Nothing  -> pure ()
        Just ref -> do
            atomicModifyIORef' ref $ \_ -> (out, ())
            notify
