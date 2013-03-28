
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Chan hiding (isEmptyChan)
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
import System.Posix.Daemonize
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Types
import System.Process
import Data.Text

import Deprecated
import Util

specialFile specialName repoRoot =
  addExtension (repoRoot </> ".cabal-dev-build-daemon") specialName

triggerFile = specialFile "trigger"

outputFile = specialFile "output"

pidFile = specialFile "pid"

statusFile = specialFile "status"

touchPred :: FilePath -> Event -> Bool
touchPred fp (Added fp' _) = fp == fp'
touchPred fp (Modified fp' _) = fp == fp'
touchPred _ _ = False

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
    ["start"]  -> start daemonize
    ["debug"]  -> start id
    ["stop"]   -> stop
    ["build"]  -> start daemonize >> waitForBuild
    ["watch"]  -> start daemonize >> watchBuilds
    ["latest"] -> start daemonize >> latestBuild
    _         -> do
      putStrLn "Usage: cabal-dev-build-daemon (start | build | stop)"
      exitFailure

prepare :: IO (FilePath, Maybe CPid)
prepare = do
  dir <- getRepoRoot
  let repoRoot = fromString dir
  let pidFilePath = encodeString $ pidFile repoRoot
  pidFileExists <- doesFileExist pidFilePath
  case pidFileExists of
    False -> return (repoRoot, Nothing)
    True -> do
      pidString <- readFile pidFilePath
      let pid = readMaybe pidString
      return (repoRoot, pid)

start :: (IO () -> IO ()) -> IO ()
start f = do
  (repoRoot, pid) <- prepare
  when (pid == Nothing) $ do
    let writePid = writeFile (encodeString $ pidFile repoRoot)
    let handler sig = CatchOnce $ writePid "" >> raiseSignal sig
    let exitOn sig = void $ installHandler sig (handler sig) Nothing
    exitOn sigTERM
    exitOn sigINT
    getProcessID >>= writePid . show
    pkg <- getPkgDesc $ encodeString repoRoot
    let srcs = maybe ["."] srcDirs pkg
    f $ withManager $ \mgr -> do
      chan <- newChan
      let w dir = watchTreeChan mgr (repoRoot </> fromString dir) eventPred chan
      mapM_ w srcs
      watchDirChan mgr repoRoot (touchPred $ triggerFile repoRoot) chan
      forever $ flushChan chan >> build repoRoot

flushChan :: Chan a -> IO ()
flushChan chan = r >> f
  where
    r = void $ readChan chan
    f = isEmptyChan chan >>= \e -> case e of
      True  -> return ()
      False -> flushChan chan

build :: FilePath -> IO ()
build repoRoot = do
  outHandle <- openFile (encodeString $ outputFile repoRoot) WriteMode
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
  let writeCode = writeFile (encodeString $ statusFile repoRoot) . show
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
waitForBuild = do
  (repoRoot, _) <- prepare
  withManager $ \mgr -> do
    chan <- newChan
    watchDirChan mgr repoRoot (touchPred $ statusFile repoRoot) chan
    writeFile (encodeString $ triggerFile repoRoot) ""
    void $ readChan chan
    readFile (encodeString $ outputFile repoRoot) >>= putStr

watchBuilds :: IO ()
watchBuilds = undefined

latestBuild :: IO ()
latestBuild = undefined

