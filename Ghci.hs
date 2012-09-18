
module Main where

import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Language.Haskell.Extension
import System.Directory
import System.Environment
import System.Exit
import System.Posix.Files
import System.Process

usage :: IO a
usage = do
  runProcess "ghci" ["--help"] Nothing Nothing Nothing Nothing Nothing
    >>= waitForProcess
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  dir <- readProcess "git" ["rev-parse", "--show-toplevel"] ""
  files <- getDirectoryContents dir
  case filter (".cabal" `isSuffixOf`) $ files of
    [cabalFile] -> do
      let cabalDevDir = dir ++ "/cabal-dev"
      hasCabalDev <- fileExist cabalDevDir
      let cabalDevDir' = if hasCabalDev then Just cabalDevDir else Nothing
      pkg <- readPackageDescription silent $ dir ++ "/" ++ cabalFile
      run (flattenPackageDescription pkg) cabalDevDir' args
    _ -> usage

run :: PackageDescription -> Maybe FilePath -> [FilePath] -> IO ()
run pkg cabalDevDir extras = do
  cdos <- maybe (return []) cabalDevOptions cabalDevDir
  let opts = srcs : cdos ++ exts ++ extras
  runProcess "ghci" opts Nothing Nothing Nothing Nothing Nothing
    >>= waitForProcess >>= exitWith
  where
    libs = maybeToList . fmap libBuildInfo . library $ pkg
    execs = map buildInfo . executables $ pkg
    infos = libs ++ execs
    exts = map extOption . concatMap defaultExtensions $ infos
    srcs = ("-i.:" ++) . intercalate ":" . concatMap hsSourceDirs $ infos

extOption :: Extension -> String
extOption (EnableExtension e) = "-X" ++ show e
extOption (DisableExtension e) = "-XNo" ++ show e
extOption (UnknownExtension xs) = "-X" ++ xs

cabalDevOptions :: FilePath -> IO [String]
cabalDevOptions dir = do
  xs <- readProcess "find" [dir, "-name", "packages-*.conf"] ""
  case lines xs of
    [] -> return []
    (conf : _) -> return ["-package-conf", conf, "-no-user-package-conf"]

