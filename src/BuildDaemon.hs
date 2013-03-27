
module Main where

import Distribution.PackageDescription
import System.FSNotify ()
import System.Posix.Daemonize
import System.Process ()

import Util

srcDirs :: PackageDescription -> [String]
srcDirs = todo

main :: IO ()
main = do
  -- 1. Get list of source directories
  dir <- getRepoRoot
  pkg <- getPkgDesc dir
  let srcs = maybe ["."] srcDirs pkg
  -- 2. Watch for new/changed files
  _ <- todo srcs
  -- 3. Run cabal-dev build and send output to a file
  todo daemonize

