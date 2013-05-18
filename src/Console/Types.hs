
{-# LANGUAGE TemplateHaskell #-}
module Console.Types where

import Data.Lens.Template
import Graphics.Vty

data Console = Console
  { _cTitle    :: String
  , _cProgress :: Maybe Rational
  , _cBody     :: String
  , _iTitle    :: Image
  , _iProgress :: Image
  , _iBody     :: Image
  , _cVty      :: Vty
  }

mkConsole :: Vty -> Console
mkConsole = Console "" Nothing "" empty_image empty_image empty_image

makeLenses [''Console]

