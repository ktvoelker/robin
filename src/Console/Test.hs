
module Main where

import Control.Monad.Trans
import Control.Monad.Trans.Resource

import Console

main :: IO ()
main = runResourceT $ do
  console <- newConsole
  let w = liftIO . cWrite console
  w $ setTitle "Some title"
  w $ setProgress . Just $ (0.4 :: Rational)
  liftIO (readFile "doc/todo") >>= w . setBody
  liftIO $ cRead console >>= print

