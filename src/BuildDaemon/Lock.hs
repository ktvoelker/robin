
module BuildDaemon.Lock (newLock, findLock, withLock) where

import Control.Exception.Lifted
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Functor
import System.Posix.Semaphore

import BuildDaemon.Types

name :: String
name = "/cabal-dev-build-daemon"

opener :: Bool -> IO Semaphore
opener create = semOpen name flags 0x700 1
  where
    flags = OpenSemFlags { semCreate = create, semExclusive = False }

newLock :: I Semaphore
-- TODO how do we detect errors here?
newLock = snd <$> allocate (opener True) (const $ semUnlink name)

findLock :: I Semaphore
-- TODO how do we detect errors here?
findLock = liftIO $ opener False

withLock :: (MonadBaseControl IO m, MonadIO m) => Semaphore -> m a -> m a
withLock sem m = bracket_ (liftIO $ semWait sem) (liftIO $ semPost sem) m

