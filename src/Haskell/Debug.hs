
module Haskell.Debug where

import Debug.Trace (trace)
import Control.Exception ( assert )

debug = do
  print "debug"

traceDebug :: a -> a
traceDebug = trace "traceDebug called"


-- Library:
-- https://github.com/ndmitchell/debug