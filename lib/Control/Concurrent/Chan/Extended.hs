module Control.Concurrent.Chan.Extended
    ( module Control.Concurrent.Chan
    , withMapChan
    ) where

import qualified Control.Concurrent.Async as Async
import           Control.Concurrent.Chan
import           Control.Monad            (forever)

withMapChan :: (a -> b) -> Chan a -> (Chan b -> IO r) -> IO r
withMapChan f chan cont = do
    new <- newChan
    Async.withAsync
        (forever $ readChan chan >>= writeChan new . f)
        (\_ -> cont new)
