
{-# LANGUAGE OverloadedStrings #-}
module BuildDaemon.Watch
  ( WatchPred(..)
  , Watch(..)
  , emptyWatch
  , watchOnce
  , watchForever
  ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan hiding (isEmptyChan)
import Control.Exception.Lifted
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Filesystem.Path.CurrentOS
import GHC.Exts (fromString)
import System.FSNotify

import BuildDaemon.Types
import Deprecated

data WatchPred =
    PredPath FilePath
  | PredName String
  | PredExtension String
  | PredAdded
  | PredModified
  | PredRemoved
  | PredConst Bool
  | PredConj [WatchPred]
  | PredDisj [WatchPred]
  | PredInverse WatchPred
  deriving (Show)

data Watch =
  Watch
  { wRecurse   :: Bool
  , wRootDir   :: FilePath
  , wPred      :: WatchPred
  } deriving (Show)

emptyWatch fp = Watch False fp (PredConst True)

eventFile :: Event -> FilePath
eventFile (Added fp _) = fp
eventFile (Modified fp _) = fp
eventFile (Removed fp _) = fp

wEvalPred :: Event -> WatchPred -> Bool
wEvalPred e (PredPath fp) = fp == eventFile e
wEvalPred e (PredName xs) = fromString xs == filename (eventFile e)
wEvalPred e (PredExtension xs) = Just (fromString xs) == extension (eventFile e)
wEvalPred (Added _ _) PredAdded = True
wEvalPred (Modified _ _) PredModified = True
wEvalPred (Removed _ _) PredRemoved = True
wEvalPred _ (PredConst b) = b
wEvalPred e (PredConj ps) = all (wEvalPred e) ps
wEvalPred e (PredDisj ps) = any (wEvalPred e) ps
wEvalPred e (PredInverse p) = not $ wEvalPred e p
wEvalPred _ _ = False

watchPrim
  :: (MonadBaseControl IO m, MonadIO m)
  => [Watch] -> (Chan Event -> m a) -> m a
watchPrim xs f = bracket (liftIO startManager) (liftIO . stopManager) $ \mgr -> do
  mapM_ (debugs . show) xs
  chan <- liftIO newChan
  let r x = if wRecurse x then watchTreeChan else watchDirChan
  let w x = r x mgr (wRootDir x) (\e -> wEvalPred e $ wPred x) chan
  liftIO $ mapM_ w xs
  f chan

readEvent :: (MonadIO m) => Chan Event -> m Event
readEvent chan = do
  event <- liftIO $ readChan chan
  debugs $ show event
  return event

watchOnce
  :: (MonadBaseControl IO m, MonadIO m)
  => [Watch] -> m a -> m a
watchOnce xs m = watchPrim xs $ \chan -> void (readEvent chan) >> m

watchForever
  :: (MonadBaseControl IO m, MonadIO m)
  => [Watch] -> m () -> m ()
watchForever xs m = watchPrim xs $ \chan -> m >> forever (flushChan chan >> m)

flushChan :: (MonadIO m) => Chan Event -> m ()
flushChan chan = r >> f
  where
    r = readEvent chan
    f = liftIO (isEmptyChan chan) >>= \e -> case e of
      True  -> return ()
      False -> flushChan chan

