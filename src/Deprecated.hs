
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Deprecated where

import qualified Control.Concurrent.Chan as Chan

isEmptyChan :: Chan.Chan a -> IO Bool
isEmptyChan = Chan.isEmptyChan

