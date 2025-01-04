--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Patat.Eval
    ( parseEvalBlocks

    , evalVar
    , evalActiveVars
    , evalAllVars
    ) where


--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async    as Async
import           Control.Exception           (IOException, catch, finally)
import           Control.Monad               (foldM, when)
import           Control.Monad.State         (StateT, runStateT, state)
import           Control.Monad.Writer        (Writer, runWriter, tell)
import           Data.Foldable               (for_)
import qualified Data.HashMap.Strict         as HMS
import qualified Data.IORef                  as IORef
import           Data.List                   (foldl')
import           Data.Maybe                  (maybeToList)
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Patat.Eval.Internal
import           Patat.Presentation.Internal
import           Patat.Presentation.Syntax
import           Patat.Unique
import           System.Exit                 (ExitCode (..))
import qualified System.IO                   as IO
import qualified System.Process              as Process


--------------------------------------------------------------------------------
parseEvalBlocks :: Presentation -> Presentation
parseEvalBlocks presentation =
    let ((pres, varGen), evalBlocks) = runWriter $
            runStateT work (pUniqueGen presentation) in
    pres {pEvalBlocks = evalBlocks, pUniqueGen = varGen}
  where
    work = case psEval (pSettings presentation) of
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
-- | Monad used for identifying and extracting the evaluation blocks from a
-- presentation.
type ExtractEvalM a = StateT UniqueGen (Writer (HMS.HashMap Var EvalBlock)) a


--------------------------------------------------------------------------------
evalSlide :: EvalSettingsMap -> Slide -> ExtractEvalM Slide
evalSlide settings slide = case slideContent slide of
    TitleSlide _ _ -> pure slide
    ContentSlide blocks -> do
        blocks1 <- dftBlocks (evalBlock settings) (pure . pure) blocks
        pure slide {slideContent = ContentSlide blocks1}


--------------------------------------------------------------------------------
evalBlock
    :: EvalSettingsMap -> Block
    -> ExtractEvalM [Block]
evalBlock settings orig@(CodeBlock attr@(_, classes, _) txt)
    | [s@EvalSettings {..}] <- lookupSettings classes settings = do
        var <- Var <$> state freshUnique
        tell $ HMS.singleton var $ EvalBlock s attr txt Nothing
        case (evalFragment, evalReplace) of
            (False, True) -> pure [VarBlock var]
            (False, False) -> pure [orig, VarBlock var]
            (True, True) -> do
                revealID <- RevealID <$> state freshUnique
                pure $ pure $ Reveal ConcatWrapper $ RevealSequence
                    revealID
                    [revealID]
                    [ (S.singleton 0, [orig])
                    , (S.singleton 1, [VarBlock var])
                    ]
            (True, False) -> do
                revealID <- RevealID <$> state freshUnique
                pure $ pure $ Reveal ConcatWrapper $ RevealSequence
                    revealID
                    [revealID]
                    [ (S.fromList [0, 1], [orig])
                    , (S.fromList [1], [VarBlock var])
                    ]
    | _ : _ : _ <- lookupSettings classes settings =
        let msg = "patat eval matched multiple settings for " <>
                T.intercalate "," classes in
        pure [CodeBlock attr msg]
evalBlock _ block =
    pure [block]


--------------------------------------------------------------------------------
newAccum :: Monoid m => (m -> IO ()) -> IO (m -> IO ())
newAccum f = do
    ref <- IORef.newIORef mempty
    pure $ \x ->
        IORef.atomicModifyIORef' ref (\y -> let z = y <> x in (z, z)) >>= f


--------------------------------------------------------------------------------
evalVar :: Var -> ([Block] -> IO ()) -> Presentation -> IO Presentation
evalVar var writeOutput presentation = case HMS.lookup var evalBlocks of
    Nothing -> pure presentation
    Just EvalBlock {..} | Just _ <- ebAsync -> pure presentation
    Just eb@EvalBlock {..} -> do
        let EvalSettings {..} = ebSettings

        writeChunk <- newAccum (writeOutput . renderEvalBlock eb)
        let drainLines copy h = do
                c <- catch (T.hGetChunk h) ((\_ -> pure "") :: IOException -> IO T.Text)
                when (c /= "") $ do
                    when copy $ writeChunk c
                    drainLines copy h

        let proc = (Process.shell $ T.unpack evalCommand)
                { Process.std_in  = Process.CreatePipe
                , Process.std_out = Process.CreatePipe
                , Process.std_err = Process.CreatePipe
                }
        (Just hIn, Just hOut, Just hErr, hProc) <- Process.createProcess proc
        async <- Async.async $
            Async.withAsync (T.hPutStr hIn ebInput `finally` IO.hClose hIn) $ \_ ->
            Async.withAsync (drainLines True hOut) $ \outAsync ->
            Async.withAsync (drainLines evalStderr hErr) $ \errAsync ->
            Async.withAsync (Process.waitForProcess hProc) $ \exitCodeAsync -> do
            erExitCode <- Async.wait exitCodeAsync
            _ <- Async.wait outAsync
            _ <- Async.wait errAsync
            case erExitCode of
                ExitSuccess -> pure ()
                ExitFailure i -> writeChunk $
                    evalCommand <> ": exit code " <> T.pack (show i) <> "\n"
        pure presentation
            { pEvalBlocks = HMS.insert var eb {ebAsync = Just async} evalBlocks
            }
  where
    evalBlocks = pEvalBlocks presentation



--------------------------------------------------------------------------------
evalActiveVars
    :: (Var -> [Block] -> IO ()) -> Presentation -> IO Presentation
evalActiveVars update presentation = foldM
    (\p var -> evalVar var (update var) p)
    presentation
    (activeVars presentation)


--------------------------------------------------------------------------------
evalAllVars :: Presentation -> IO Presentation
evalAllVars pres = do
    updates <- IORef.newIORef []

    let forceEvalVar pres0 var = do
            pres1 <- evalVar
                var
                (\u -> IORef.atomicModifyIORef' updates (\l -> (l ++ [u], ())))
                pres0
            case HMS.lookup var (pEvalBlocks pres1) of
                Nothing -> pure pres1
                Just eb -> do
                    for_ (ebAsync eb) Async.wait
                    IORef.atomicModifyIORef' updates $ \l ->
                        ([], foldl' (\p u -> updateVar var u p) pres1 l)

    foldM forceEvalVar pres (HMS.keys (pEvalBlocks pres))
