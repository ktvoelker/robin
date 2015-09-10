
module Util (todo, leftMaybe, readMaybe, getRepoRoot) where

import Data.Char
import System.Process

todo :: a
todo = error "Not implemented"

leftMaybe :: Either a b -> Maybe a
leftMaybe = either Just $ const Nothing

readMaybe :: (Read a) => String -> Maybe a
readMaybe xs = case reads xs of
  [(x, "")] -> Just x
  _ -> Nothing

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

getRepoRoot :: IO String
getRepoRoot = readProcess "git" ["rev-parse", "--show-toplevel"] "" >>= return . trim

