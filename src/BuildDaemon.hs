
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import Data.Lens
import Filesystem.Path.CurrentOS
import System.Environment
import System.Exit
import System.IO
import System.Posix.Daemonize
import System.Posix.Directory
import System.Posix.IO
import System.Posix.Semaphore
import System.Posix.Signals
import System.Process

import BuildDaemon.Lock
import BuildDaemon.PidFile
import BuildDaemon.Types
import BuildDaemon.Watch
import Util

check :: Either Err a -> IO ()
check = maybe (return ()) print . leftMaybe

startDaemon :: IO ()
startDaemon = do
  debug "Starting daemon..."
  wd <- getWorkingDirectory
  daemonize $ do
    changeWorkingDirectory wd
    (check =<<) . runI $ do
      logFileString <- asksString envLogFile
      let
      { openLog = do
          (hKey, logHandle) <- allocate (openFile logFileString AppendMode) hClose
          (fdKey, logFd) <- allocate (handleToFd logHandle) closeFd
          void $ unprotect hKey
          void $ allocate (dupTo logFd stdOutput) closeFd
          release fdKey
      }
      openLog
      debug "Daemon started; log opened."
      start

usage :: IO ()
usage = hPutStrLn stderr
  "Usage: cabal-dev-build-daemon (start | stop | build | watch | debug)"

commands :: [(String, IO ())]
commands =
  [ ("start" , startDaemon)
  , ("debug" , runI start >>= check)
  , ("stop"  , runI stop >>= check)
  , ("build" , startDaemon >> runM waitForBuild >>= check)
  , ("watch" , startDaemon >> runM watchBuilds >>= check)
  , ("help"  , usage)
  ]

errExit :: IO ()
errExit = usage >> exitFailure

command :: [String] -> IO ()
command []     = command ["build"]
command [name] = maybe errExit id $ lookup name commands
command _      = errExit

main :: IO ()
main = fst (startDaemon, getArgs >>= command)

exts :: [String]
exts = ["hs", "lhs", "cabal"]

srcPred :: WatchPred
srcPred = PredDisj $ map PredExtension exts

start :: I ()
start = do
  repoRoot <- asks (envRepoRoot ^$)
  readPidFile >>= \pid -> case pid of
    Just pid -> do
      debugs $ "Daemon already running: pid " ++ show pid
    Nothing -> do
      debug "Creating lock..."
      sem <- newLock
      debug "Starting notification manager..."
      do
        mainThread <- liftIO myThreadId
        liftIO $ do
          let
          { handler = Catch $ do
              debug "Caught signal; throwing ExitSuccess to main thread"
              throwTo mainThread $ ExitSuccess
          }
          let exitOn sig = void $ installHandler sig handler Nothing
          debug "Installing signal handlers..."
          exitOn sigTERM
          exitOn sigINT
        debug "Writing pidfile..."
        writePidFile
        debug "Starting event loop..."
        let
        { w = (emptyWatch $ repoRoot)
          { wRecurse  = True
          , wPred     = srcPred
          }
        }
        watchForever [w] $ build sem

build :: Semaphore -> I ()
build sem = withLock sem $ do
  repoRoot <- asksString envRepoRoot
  outHandle <- asksString envOutputFile >>= liftIO . flip openFile WriteMode
  code <- liftIO $ do
    (_, _, _, ph) <-
      createProcess
      $ (proc "cabal-dev" ["build"])
        { cwd = Just repoRoot
        , std_in = Inherit
        , std_out = Inherit
        , std_err = UseHandle outHandle
        }
    hClose outHandle
    waitForProcess ph
  case code of
    ExitSuccess -> writeCode 0
    ExitFailure n -> writeCode n

writeCode :: (MonadReader Env m, MonadIO m) => Int -> m ()
writeCode n = asksString envStatusFile >>= liftIO . flip writeFile (show n)

stop :: I ()
stop = do
  pid <- readPidFile
  case pid of
    Nothing -> return ()
    Just pid -> liftIO $ signalProcess sigTERM pid

waitForBuild :: M ()
waitForBuild = do
  (liftIO . exitWith =<<) . (lift findLock >>=) . flip withLock $ do
    asksString envOutputFile >>= liftIO . readFile >>= liftIO . putStr
    statusStr <- asksString envStatusFile >>= liftIO . readFile
    return $ case readMaybe statusStr of
      Nothing -> ExitFailure 42
      Just 0  -> ExitSuccess
      Just n  -> ExitFailure n

watchBuilds :: M ()
watchBuilds = do
  statusFile <- asks (envStatusFile ^$)
  let
  { w = (emptyWatch $ directory statusFile)
    { wPred = PredDisj [PredPath statusFile, PredInverse PredRemoved]
    }
  }
  watchOnce [w] lessDump

lessDump :: M ()
lessDump = void $
  access stLessProcess >>= maybe (return ()) (liftIO . terminateProcess)
  >>
  dump >>= (stLessProcess ~=) . Just

clearTerminal :: (MonadIO m) => m ProcessHandle
clearTerminal = liftIO $ runProcess "clear" [] Nothing Nothing Nothing Nothing Nothing

dump :: M ProcessHandle
dump = do
  clearProc <- clearTerminal
  lessFileString <- asksString envLessFile
  (lift findLock >>=) . flip withLock
    $ asksString envRepoRoot
      >>= liftIO . readFile
      >>= liftIO . writeFile lessFileString
  outHandle <- liftIO $ openFile lessFileString ReadMode
  void $ liftIO $ waitForProcess clearProc
  lessProc <-
    liftIO
    $ runProcess "less" [] Nothing Nothing (Just outHandle) Nothing Nothing
  liftIO $ hClose outHandle
  return lessProc

