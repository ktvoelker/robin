
{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module BuildDaemon.Types where

import Prelude hiding (FilePath, putStrLn)

import Control.Monad.Except
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
import System.Process

import Util

data St =
  St
  { _stLessProcess :: Maybe ProcessHandle
  }

emptySt :: St
emptySt = St Nothing

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

emptyEnv :: String -> Env
emptyEnv rr =
  Env
  { _envRepoRoot   = rr'
  , _envStatusFile = specialFile "status"
  , _envOutputFile = specialFile "output"
  , _envLessFile   = specialFile "less"
  , _envPidFile    = specialFile "pid"
  , _envLogFile    = specialFile "log"
  }
  where
    rr' = fromString rr
    specialFile n = addExtension (rr' </> ".cabal-dev-build-daemon") n

data Err = Err
  deriving (Show)

type I = ReaderT Env (ExceptT Err (ResourceT IO))

type M = StateT St I

runI :: I a -> IO (Either Err a)
runI m = getRepoRoot >>= runResourceT . runExceptT . runReaderT m . emptyEnv

runM :: M a -> IO (Either Err a)
runM = runI . flip evalStateT emptySt

asksString :: (MonadReader Env m) => Lens Env FilePath -> m String
asksString lens = asks $ encodeString . (lens ^$)

debug :: (MonadIO m) => Text -> m ()
debug text = liftIO $ putStrLn text >> hFlush stdout

debugs :: (MonadIO m) => String -> m ()
debugs = debug . fromString

