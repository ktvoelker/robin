
{-# LANGUAGE TemplateHaskell #-}
module Console.Types where

import Data.Lens.Template
import Graphics.Vty

data BuildState = BuildSuccess | BuildFailure | BuildRunning

data Console = Console
  { _cTitle    :: String
  , _cProgress :: Maybe Rational
  , _cBodyPrev :: [String]
  , _cBody     :: [String]
  , _cState    :: BuildState
  , _iTitle    :: Image
  , _iProgress :: Image
  , _iBody     :: Image
  , _iScroll   :: Image
  , _cVty      :: Vty
  }

mkConsole :: Vty -> Console
mkConsole =
  Console "" Nothing [] [] BuildRunning
    empty_image empty_image empty_image empty_image

makeLenses [''Console]

