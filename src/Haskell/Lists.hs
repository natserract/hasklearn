module Haskell.Lists where

-- trace: for debugging
import Debug.Trace (trace)

lists = do
  basicLists

basicLists :: [[Char]] -> IO ()
basicLists [] = error "Lists is empty!" -- Check if nill/empty!
basicLists ["Infinite"] = print "First index is Infinite" -- Check if length === 0 && "Infinite"
basicLists (t : rest) = trace "Lists called" print t

aliasLists :: ([Int], [Char]) -> String
aliasLists ([], x) = show 0
aliasLists (p@(t : res), x) 
  | x == "all" = show p -- [1,2]
  | x == "head" = show t -- [1]
  | x == "tail" = show res -- [2]
  | otherwise = show [0]