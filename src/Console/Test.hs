
module Main where

import Control.Monad

import Console

main :: IO ()
main = runCM $ do
  setTitle "Some title"
  setProgress . Just $ (0.4 :: Rational)
  setBody "The body text\nLine 2"
  void $ nextEvent

