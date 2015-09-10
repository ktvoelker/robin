
module PidFile where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import System.Directory
import System.Posix.Process
import System.Posix.Types

import Types
import Util

readPidFile :: (MonadIO m, MonadReader Env m) => m (Maybe CPid)
readPidFile = do
  view envPidFile
  >>= \fp -> liftIO (doesFileExist fp)
  >>= \ex -> case ex of
    False -> return Nothing
    True  -> liftIO (readFile fp) >>= return . readMaybe

requirePidFile :: (MonadIO m, MonadReader Env m, MonadError Err m) => m CPid
requirePidFile = readPidFile >>= maybe (throwError Err) return

writePidFile :: (MonadIO m, MonadReader Env m, MonadResource m) => m ()
writePidFile = do
  fp  <- view envPidFile
  pid <- liftIO getProcessID
  _   <- allocate (writeFile fp $ show pid) (const $ removeFile fp)
  return ()

