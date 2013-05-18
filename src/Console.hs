
module Console where

import Control.Applicative
import Control.Monad.Trans.Resource
import Control.Monad.State
import Data.Lens
import Graphics.Vty

import Console.Types

type CM a = StateT Console (ResourceT IO) a

runCM :: CM a -> IO a
runCM m = runResourceT $ do
  (key, vty) <- allocate mkVty shutdown
  evalStateT m (mkConsole vty) <* release key

rebuild :: CM ()
rebuild = undefined

setTitle :: String -> CM ()
setTitle xs = do
  void $ cTitle ~= xs
  void $ iTitle ~= string def_attr xs
  rebuild

setProgress :: (Fractional a) => a -> CM ()
setProgress _ = do
  void $ cProgress ~= undefined
  void $ iProgress ~= undefined
  rebuild

setBody :: String -> CM ()
setBody xs = do
  void $ cBody ~= xs
  void $ iBody ~= undefined
  rebuild

