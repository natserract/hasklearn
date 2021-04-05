module Haskell.Lists where

import Debug.Trace (trace)
lists :: [[Char]] -> IO ()
lists [] = error "Lists is empty!" -- Check if nill!
lists ["Infinite"] = print "Nilai List pertama tidak diketahui" -- Check if length === 0 && "Infinite"
lists (t : rest) = trace "Lists called" print t