
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module BuildDaemon.Types where

import Prelude hiding (FilePath, putStrLn)

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Lens
import Data.Lens.Template
import Data.Text (Text())
import Data.Text.IO (putStrLn)
import Filesystem.Path.CurrentOS
import GHC.Exts (fromString)
import System.IO (hFlush, stdout)
import System.Posix.Semaphore
import System.Process

data St =
  St
  { _stSemaphore   :: Semaphore
  , _stLessProcess :: Maybe ProcessHandle
  }

emptySt sem = St sem Nothing

data Env =
  Env
  { _envRepoRoot   :: FilePath
  , _envStatusFile :: FilePath
  , _envOutputFile :: FilePath
  , _envLessFile   :: FilePath
  , _envPidFile    :: FilePath
  , _envLogFile    :: FilePath
  }
  deriving (Show)

makeLenses [''St, ''Env]

emptyEnv :: FilePath -> Env
emptyEnv rr =
  Env
  { _envRepoRoot = rr
  , _envStatusFile = specialFile "status"
  , _envOutputFile = specialFile "output"
  , _envLessFile   = specialFile "less"
  , _envPidFile    = specialFile "pid"
  , _envLogFile    = specialFile "log"
  }
  where
    specialFile n = addExtension (rr </> ".cabal-dev-build-daemon") n

data Err = Err
  deriving (Show)

instance Error Err where
  noMsg = Err
  strMsg = const Err

type I = ReaderT Env (ErrorT Err (ResourceT IO))

type M = StateT St I

runI :: FilePath -> I a -> IO (Either Err a)
runI repoRoot m = runResourceT $ runErrorT $ runReaderT m $ emptyEnv repoRoot

runM :: FilePath -> I St -> M a -> IO (Either Err a)
runM repoRoot init action = runI repoRoot $ init >>= evalStateT action

asksString :: (MonadReader Env m) => Lens Env FilePath -> m String
asksString lens = asks $ encodeString . (lens ^$)

debug :: (MonadIO m) => Text -> m ()
debug text = liftIO $ putStrLn text >> hFlush stdout

debugs :: (MonadIO m) => String -> m ()
debugs = debug . fromString

