
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Chan
import Control.Monad
import Distribution.PackageDescription
import Filesystem.Path.CurrentOS
import GHC.Exts (fromString)
import Prelude hiding (FilePath)
import System.Directory
import System.Environment
import System.Exit
import System.FSNotify
import System.IO hiding (FilePath)
import System.Posix.Daemonize ()
import System.Posix.Signals
import System.Posix.Types
import System.Process
import Data.Text

import Util

srcDirs :: PackageDescription -> [String]
srcDirs = buildInfos >=> hsSourceDirs

exts :: [Text]
exts = ["hs", "lhs"]

eventPred :: Event -> Bool
eventPred = maybe False (`elem` exts) . extension . eventFile

eventFile :: Event -> FilePath
eventFile (Added fp _) = fp
eventFile (Modified fp _) = fp
eventFile (Removed fp _) = fp

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["start"] -> start
    ["stop"]  -> stop
    ["build"] -> start >> waitForBuild
    _         -> do
      putStrLn "Usage: cabal-dev-build-daemon (start | build | stop)"
      exitFailure

prepare :: IO (FilePath, Maybe CPid)
prepare = do
  dir <- getRepoRoot
  let repoRoot = fromString dir
  let pidFile = encodeString $ repoRoot </> ".cabal-dev-build-pid"
  pidFileExists <- doesFileExist pidFile
  case pidFileExists of
    False -> return (repoRoot, Nothing)
    True -> do
      pidString <- readFile pidFile
      let pid = readMaybe pidString
      return (repoRoot, pid)

start :: IO ()
start = do
  (repoRoot, pid) <- prepare
  when (pid == Nothing) $ do
    let codeFile = repoRoot </> ".cabal-dev-build-status"
    let outFile = repoRoot </> ".cabal-dev-build-output"
    pkg <- getPkgDesc $ encodeString repoRoot
    let srcs = maybe ["."] srcDirs pkg
    -- TODO daemonize everything below here
    withManager $ \mgr -> do
      chan <- newChan
      let w dir = watchTreeChan mgr (repoRoot </> fromString dir) eventPred chan
      mapM_ w srcs
      -- TODO read everything that is already waiting in the chan and then build once
      -- unless we read nothing
      forever $ readChan chan >>= const (build repoRoot codeFile outFile)

build :: FilePath -> FilePath -> FilePath -> IO ()
build repoRoot codeFile outFile = do
  outHandle <- openFile (encodeString outFile) WriteMode
  (_, _, _, ph) <-
    createProcess
    $ (proc "cabal-dev" ["build"])
      { cwd = Just $ encodeString repoRoot
      , std_in = Inherit
      , std_out = Inherit
      , std_err = UseHandle outHandle
      }
  hClose outHandle
  code <- waitForProcess ph
  let writeCode = writeFile (encodeString codeFile) . show
  case code of
    ExitSuccess -> writeCode 0
    ExitFailure n -> writeCode n

stop :: IO ()
stop = do
  (_, pid) <- prepare
  case pid of
    Nothing -> return ()
    Just pid -> signalProcess sigTERM pid

waitForBuild :: IO ()
waitForBuild = todo
-- 1. Set up fsnotify to watch for a change to .cabal-dev-build-status
-- 2. Touch .cabal-dev-build-trigger (make the daemon watch it)
-- 3. Once .cabal-dev-build-status changes, print .cabal-dev-build-output

