module Basic.Print where

printFmt = do
  fmt 
  fmtChar ("Alfin", "Surya")
  putStrFn
  putStrLnFn

-- any type
fmt = print "Message from fmt"

fmtChar :: (String, String) -> IO ()
fmtChar (a, b) = print (a ++ b) -- () -> ([a0], [a0])

-- String type 
putStrFn = putStr "Message from putStr"

-- String type w newline
-- The same as putStr, but adds a newline char
putStrLnFn = putStrLn "Message from putStrLnFunc"
