
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
import System.IO.Error
import System.Posix.Daemonize
import System.Posix.Process
import System.Posix.Semaphore
import System.Posix.Signals
import System.Posix.Types
import System.Process
import Data.Text

import Deprecated
import Util

specialFile specialName repoRoot =
  addExtension (repoRoot </> ".cabal-dev-build-daemon") specialName

outputFile = specialFile "output"

pidFile = specialFile "pid"

statusFile = specialFile "status"

semName = "/cabal-dev-build-daemon"

getSem :: Bool -> IO Semaphore
getSem create =
  semOpen semName (f create) 0x700 1
  where
    f create = OpenSemFlags { semCreate = create, semExclusive = False }

withSem :: Semaphore -> IO a -> IO a
withSem sem action = do
  semWait sem
  ret <- tryIOError action
  semPost sem
  case ret of
    Left err -> ioError err
    Right ret -> return ret

withGetSem :: IO a -> IO a
withGetSem action = getSem False >>= \sem -> withSem sem action

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
  putStrLn $ "Found repo root: " ++ encodeString repoRoot
  case pid of
    Just pid -> putStrLn $ "Daemon already running: pid " ++ show pid
    Nothing -> do
      let pidFileString = encodeString $ pidFile repoRoot
      let writePid = writeFile pidFileString
      let rmPid = removeFile pidFileString
      let handler sig = CatchOnce $ rmPid >> raiseSignal sig
      let exitOn sig = void $ installHandler sig (handler sig) Nothing
      putStrLn "Installing signal handlers..."
      exitOn sigTERM
      exitOn sigINT
      putStrLn "Writing pidfile..."
      getProcessID >>= writePid . show
      flip catchIOError (\err -> rmPid >> print err) $ do
        putStrLn "Reading package description..."
        pkg <- getPkgDesc $ encodeString repoRoot
        let srcs = maybe ["."] srcDirs pkg
        putStrLn "Creating semaphore..."
        sem <- getSem True
        putStrLn "Starting notification manager..."
        f $ withManager $ \mgr -> do
          chan <- newChan
          let w dir = watchTreeChan mgr (repoRoot </> fromString dir) eventPred chan
          mapM_ w srcs
          putStrLn "Starting initial build..."
          build sem repoRoot
          putStrLn "Starting event loop..."
          forever $ flushChan chan >> build sem repoRoot

flushChan :: Chan a -> IO ()
flushChan chan = r >> f
  where
    r = void $ readChan chan
    f = isEmptyChan chan >>= \e -> case e of
      True  -> return ()
      False -> flushChan chan

build :: Semaphore -> FilePath -> IO ()
build sem repoRoot = withSem sem $ do
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
  status <- withGetSem $ do
    readFile (encodeString $ outputFile repoRoot) >>= putStr
    statusStr <- readFile (encodeString $ statusFile repoRoot)
    return $ case readMaybe statusStr of
      Nothing -> ExitFailure 42
      Just 0  -> ExitSuccess
      Just n  -> ExitFailure n
  exitWith status

watchBuilds :: IO ()
watchBuilds = undefined

