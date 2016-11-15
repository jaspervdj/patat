--------------------------------------------------------------------------------
module Patat.AutoAdvance
    ( autoAdvance
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent      (forkIO, threadDelay)
import qualified Control.Concurrent.Chan as Chan
import           Control.Monad           (forever)
import qualified Data.IORef              as IORef
import           Data.Time               (diffUTCTime, getCurrentTime)
import           Patat.Presentation      (PresentationCommand (..))


--------------------------------------------------------------------------------
-- | This function takes an existing channel for presentation commands
-- (presumably coming from human input) and creates a new one that /also/ sends
-- a 'Forward' command if nothing happens for N seconds.
autoAdvance
    :: Int
    -> Chan.Chan PresentationCommand
    -> IO (Chan.Chan PresentationCommand)
autoAdvance delaySeconds existingChan = do
    let delay = delaySeconds * 1000  -- We are working with ms in this function

    newChan         <- Chan.newChan
    latestCommandAt <- IORef.newIORef =<< getCurrentTime

    -- This is a thread that copies 'existingChan' to 'newChan', and writes
    -- whenever the latest command was to 'latestCommandAt'.
    _ <- forkIO $ forever $ do
        cmd <- Chan.readChan existingChan
        getCurrentTime >>= IORef.writeIORef latestCommandAt
        Chan.writeChan newChan cmd

    -- This is a thread that waits around 'delay' seconds and then checks if
    -- there's been a more recent command.  If not, we write a 'Forward'.
    _ <- forkIO $ forever $ do
        current <- getCurrentTime
        latest  <- IORef.readIORef latestCommandAt
        let elapsed = floor $ 1000 * (current `diffUTCTime` latest) :: Int
        if elapsed >= delay
            then do
                Chan.writeChan newChan Forward
                IORef.writeIORef latestCommandAt current
                threadDelay (delay * 1000)
            else do
                let wait = delay - elapsed
                threadDelay (wait * 1000)

    return newChan
