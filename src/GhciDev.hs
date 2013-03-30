
module Main (main) where

import Data.List
import Distribution.PackageDescription
import Language.Haskell.Extension
import System.Environment
import System.Exit
import System.Posix.Files
import System.Process

import Util

main :: IO ()
main = do
  args <- getArgs
  dir <- getRepoRoot
  let cabalDevDir = dir ++ "/cabal-dev"
  hasCabalDev <- fileExist cabalDevDir
  let cabalDevDir' = if hasCabalDev then Just cabalDevDir else Nothing
  pkg <- getPkgDesc dir
  run pkg cabalDevDir' args

cabalFileOptions :: PackageDescription -> [String]
cabalFileOptions pkg = srcs : exts
  where
    infos = buildInfos pkg
    newExts = concatMap defaultExtensions infos
    oldExts = concatMap oldExtensions infos
    exts = map extOption $ newExts ++ oldExts
    srcs = ("-i.:" ++) . intercalate ":" . concatMap hsSourceDirs $ infos

run :: Maybe PackageDescription -> Maybe FilePath -> [FilePath] -> IO ()
run pkg cabalDevDir extras = do
  cdos <- maybe (return []) cabalDevOptions cabalDevDir
  let opts = maybe [] cabalFileOptions pkg ++ cdos ++ extras
  runProcess "ghci" opts Nothing Nothing Nothing Nothing Nothing
    >>= waitForProcess >>= exitWith

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

