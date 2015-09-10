
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Daemonize
import System.Posix.Directory
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Process

import Command
import Lock
import PidFile
import Types
import Watch
import Util

check :: Either Err a -> IO ()
check = maybe (return ()) print . leftMaybe

startDaemon :: IO ()
startDaemon = do
  wd <- getWorkingDirectory
  -- Fork before calling daemonize, because it calls exit after forking the daemon.
  void $ forkProcess $ daemonize $ do
    changeWorkingDirectory wd
    (check =<<) . runI $ do
      logFileString <- view envLogFile
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
  "Usage: cabal-build-daemon (start | stop | build | watch | debug)"

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
main = getArgs >>= command

exts :: [String]
exts = ["hs", "lhs", "cabal", "yaml"]

srcPred :: WatchPred
srcPred = PredDisj $ map PredExtension exts

start :: I ()
start = do
  repoRoot <- view envRepoRoot
  readPidFile >>= \pid -> case pid of
    Just pid -> do
      debugs $ "Daemon already running: pid " ++ show pid
    Nothing -> do
      debug "Creating lock..."
      void ensureLock
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
        watchForever [w] build

-- TODO use ResourceT to be safer about the handle
runBuildProcess :: CreateProcess -> I ExitCode
runBuildProcess p = do
  repoRoot <- view envRepoRoot
  outHandle <- view envOutputFile >>= liftIO . flip openFile WriteMode
  liftIO $ do
    (_, _, _, ph) <- createProcess $ p
      { cwd = Just repoRoot
      , std_in = Inherit
      , std_out = Inherit
      , std_err = UseHandle outHandle
      }
    code <- waitForProcess ph
    hClose outHandle
    return code

build :: I ()
build = withLock $ view envRepoRoot >>= buildCommand >>= runBuildProcess >>= \case
  ExitSuccess -> writeCode 0
  ExitFailure n -> writeCode n

writeCode :: (MonadReader Env m, MonadIO m) => Int -> m ()
writeCode n = view envStatusFile >>= liftIO . flip writeFile (show n)

stop :: I ()
stop = do
  pid <- readPidFile
  case pid of
    Nothing -> return ()
    Just pid -> liftIO $ signalProcess sigTERM pid

waitForBuild :: M ()
waitForBuild = do
  (liftIO . exitWith =<<) . withLock $ do
    view envOutputFile >>= liftIO . readFile >>= liftIO . putStr
    statusStr <- view envStatusFile >>= liftIO . readFile
    return $ case readMaybe statusStr of
      Nothing -> ExitFailure 42
      Just 0  -> ExitSuccess
      Just n  -> ExitFailure n

watchBuilds :: M ()
watchBuilds = do
  statusFile <- view envStatusFile
  let
  { w = (emptyWatch $ takeDirectory statusFile)
    { wPred = PredConj [PredPath statusFile, PredInverse PredRemoved]
    }
  }
  watchForever [w] lessDump

lessDump :: M ()
lessDump = void $
  use stLessProcess >>= maybe (return ()) (liftIO . terminateProcess)
  >> dump >>= assign stLessProcess . Just

clearTerminal :: (MonadIO m) => m ProcessHandle
clearTerminal = liftIO $ runProcess "clear" [] Nothing Nothing Nothing Nothing Nothing

dump :: M ProcessHandle
dump = do
  clearProc <- clearTerminal
  lessFileString <- view envLessFile
  withLock
    $ view envOutputFile
      >>= liftIO . readFile
      >>= liftIO . writeFile lessFileString
  outHandle <- liftIO $ openFile lessFileString ReadMode
  void $ liftIO $ waitForProcess clearProc
  lessProc <-
    liftIO
    $ runProcess "less" [] Nothing Nothing (Just outHandle) Nothing Nothing
  liftIO $ hClose outHandle
  return lessProc

