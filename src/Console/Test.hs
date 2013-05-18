
module Main where

import Control.Monad
import Control.Monad.Trans

import Console

main :: IO ()
main = runCM $ do
  setTitle "Some title"
  setProgress . Just $ (0.4 :: Rational)
  liftIO (readFile "doc/todo") >>= setBody
  void $ nextEvent
  scrollDown 5
  void $ nextEvent

