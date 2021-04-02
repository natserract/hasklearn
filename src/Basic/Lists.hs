module Basic.Lists where

lists :: [[Char]] -> IO ()
lists [] = print "Lists is empty!" -- Check if nill!
lists ["Infinite"] = print "Nilai List pertama tidak diketahui" -- Check if length === 0 && "Infinite"
lists (t : rest) = print t