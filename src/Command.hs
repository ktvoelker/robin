
module Command where

import Control.Monad.Trans
import System.Directory
import System.FilePath
import System.Process

buildCommand :: (MonadIO m) => FilePath -> m CreateProcess
buildCommand dir = liftIO $
  doesFileExist (dir </> "robin.sh") >>= \case
    True -> return $ proc "./robin.sh" []
    False -> doesFileExist (dir </> "stack.yaml") >>= \case
      True -> return $ proc "stack" ["build"]
      False -> return $ proc "cabal" ["build"]

