
module Main where

import System.Posix.Daemonize
import System.Process ()

-- 1. Get list of source directories
-- 2. Watch for new/changed files
-- 3. Run cabal-dev build and send output to a file
main :: IO ()
main = daemonize undefined

