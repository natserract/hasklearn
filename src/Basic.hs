{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PostfixOperators #-}

module Basic
  ( 
    lists,
    useLet,
  )
where

-- Simply pattern matching / inline
lists :: [[Char]] -> IO ()
lists [] = print "Lists is empty!" -- Check if nill!
lists ["Infinite"] = print "Nilai List pertama tidak diketahui" -- Check if length === 0 && "Infinite"
lists (t : rest) = print t

-- Let bindings
useLet :: Int -> Int
useLet x =
  let c = c * c in x

length' :: (Num b) => [a] -> b
length' [] = 0
length' xs = sum [1 | _ <- xs]
