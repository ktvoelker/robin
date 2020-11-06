
module Lock (ensureLock, withLock) where

import UnliftIO.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import System.Posix.Semaphore

name :: String
name = "/robin-xRBVe8IB7Z"

ensureLock :: (MonadIO m) => m Semaphore
ensureLock = liftIO $ semOpen name flags 0x700 1
  where
    flags = OpenSemFlags { semCreate = True, semExclusive = False }

withLock :: (MonadUnliftIO m, MonadIO m) => m a -> m a
withLock m =
  liftIO ensureLock
  >>= \sem -> bracket_ (liftIO $ semWait sem) (liftIO $ semPost sem) m

