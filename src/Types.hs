
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types where

import Prelude hiding (FilePath, putStrLn)

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Text (Text(), pack)
import Data.Text.IO (putStrLn)
import System.Directory
import System.IO (hFlush, stdout)
import System.FilePath
import System.Process

import Util

data St =
  St
  { _stLessProcess :: Maybe ProcessHandle
  }

makeLenses ''St

emptySt :: St
emptySt = St Nothing

data Env =
  Env
  { _envRepoRoot   :: FilePath
  , _envRobinRoot  :: FilePath
  , _envStatusFile :: FilePath
  , _envOutputFile :: FilePath
  , _envLessFile   :: FilePath
  , _envPidFile    :: FilePath
  , _envLogFile    :: FilePath
  }
  deriving (Show)

makeLenses ''Env

emptyEnv :: String -> Env
emptyEnv rr =
  Env
  { _envRepoRoot   = rr
  , _envRobinRoot  = robinRoot
  , _envStatusFile = specialFile "status"
  , _envOutputFile = specialFile "output"
  , _envLessFile   = specialFile "less"
  , _envPidFile    = specialFile "pid"
  , _envLogFile    = specialFile "log"
  }
  where
    robinRoot = rr </> ".robin"
    specialFile n = robinRoot </> n

data Err = Err
  deriving (Show)

type I = ReaderT Env (ExceptT Err (ResourceT IO))

type M = StateT St I

instance (MonadUnliftIO m) => MonadUnliftIO (ExceptT r m)
instance MonadUnliftIO m => MonadUnliftIO (StateT r m)

runI :: I a -> IO (Either Err a)
runI m = getRepoRoot >>= runResourceT . runExceptT . runReaderT m' . emptyEnv
  where
    m' = view envRobinRoot >>= ensureDir >> m

ensureDir :: (MonadIO m) => FilePath -> m ()
ensureDir = liftIO . createDirectoryIfMissing False

runM :: M a -> IO (Either Err a)
runM = runI . flip evalStateT emptySt

debug :: (MonadIO m) => Text -> m ()
debug text = liftIO $ putStrLn text >> hFlush stdout

debugs :: (MonadIO m) => [Char] -> m ()
debugs = debug . pack

