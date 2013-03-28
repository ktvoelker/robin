
module Main where

import Control.Concurrent.Chan
import Control.Monad
import Distribution.PackageDescription
import Filesystem.Path.CurrentOS
import GHC.Exts
import System.FSNotify
import System.Posix.Daemonize ()
import System.Process ()

import Util

srcDirs :: PackageDescription -> [String]
srcDirs = buildInfos >=> hsSourceDirs

main :: IO ()
main = do
  -- 1. Get list of source directories
  dir <- getRepoRoot
  pkg <- getPkgDesc dir
  let srcs = maybe ["."] srcDirs pkg
  -- TODO daemonize everything below here
  -- 2. Watch for new/changed files
  withManager $ \mgr -> do
    chan <- newChan
    let repoRoot = fromString dir
    let w dir = watchTreeChan mgr (repoRoot </> fromString dir) (const True) chan
    mapM_ w srcs
    -- 3. Run cabal-dev build and send output to a file
    forever $ readChan chan >>= print

