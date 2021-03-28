
{-# LANGUAGE ConstraintKinds #-}

module Basic (
  fmt,
  fmtChar,
  lists,
  useLet
) where

-- fmt: Using type signature
-- Features: [type *functions, concat string]
fmt :: String -> String -> IO ()
fmt a b = print (a ++ b)

fmtChar :: (String, String) -> IO ()
fmtChar (a, b) = print (a ++ b) -- () -> ([a0], [a0])

-- Simply pattern matching / inline
lists:: [[Char]] -> IO ()
lists [] = print "Lists is empty!" -- Check if nill!
lists ["Infinite"] = print "Nilai List pertama tidak diketahui" -- Check if length === 0 && "Infinite"
lists (t: rest) = print t

-- Let bindings
useLet :: Int -> Int
useLet x = 
  let c = c * c in x