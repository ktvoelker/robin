
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent.Chan
import Control.Monad
import Distribution.PackageDescription
import Filesystem.Path.CurrentOS
import GHC.Exts (fromString)
import Prelude hiding (FilePath)
import System.Exit
import System.FSNotify
import System.IO hiding (FilePath)
import System.Posix.Daemonize ()
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
  -- 1. Get list of source directories
  dir <- getRepoRoot
  let repoRoot = fromString dir
  let codeFile = repoRoot </> ".cabal-dev-build-status"
  let outFile = repoRoot </> ".cabal-dev-build-output"
  pkg <- getPkgDesc dir
  let srcs = maybe ["."] srcDirs pkg
  -- TODO daemonize everything below here
  -- 2. Watch for new/changed files
  withManager $ \mgr -> do
    chan <- newChan
    let w dir = watchTreeChan mgr (repoRoot </> fromString dir) eventPred chan
    mapM_ w srcs
    -- 3. Run cabal-dev build and send output to a file
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

