
module Util where

import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import System.Directory
import System.Process

todo :: a
todo = error "Not implemented"

buildInfos :: PackageDescription -> [BuildInfo]
buildInfos pkg = libs ++ execs
  where
    libs = maybeToList . fmap libBuildInfo . library $ pkg
    execs = map buildInfo . executables $ pkg

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

getRepoRoot :: IO String
getRepoRoot = readProcess "git" ["rev-parse", "--show-toplevel"] "" >>= return . trim

getPkgDesc :: String -> IO (Maybe PackageDescription)
getPkgDesc dir = do
  files <- getDirectoryContents dir
  case filter (".cabal" `isSuffixOf`) $ files of
    [cabalFile] ->
      Just . flattenPackageDescription
      <$> readPackageDescription silent (dir ++ "/" ++ cabalFile)
    _ -> return Nothing

