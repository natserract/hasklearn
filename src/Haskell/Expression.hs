{-# LANGUAGE BangPatterns #-}

module Haskell.Expression where

outExpr = do
  print ""
  print result
  -- print $ fst result
  -- print $ recList [1, 2, 3]


-- max n = do 
--   let n | n < 6 = 0 | otherwise = n + 1 in n

-- maxNum :: (Ord a, Num a) => [p] -> a
{--
  Recursive function fnMaxNum

-}
-- infinite list, because hasn't base case
infiniteRecList :: a -> [a]
infiniteRecList n =
  let ns = (n:ns) in ns

{-
  How it's works?

  -> let ns = .. 
    let just gives names to values.
    let untuk "mengikat simpul", membuat single (:) konstruktor yang mengacu pada dirinya sendiri. 
    use the left to select parts of the right
    I think, we select value in right =, and give an name in left, let (..), right?

    pattern matching is used to extract parts of existing data (or the whole data)

  -> (n:ns) in ns 
    n = head array, mirip seperti ini (t:res) | take the first item n in the ns

    *.js / "tail call optimization" in js
     function recList(n) {
      function ns(h, res = 0) {
        const [first, ...rest] = n;
        return ns(rest, first+res)
      }
      
      return ns(n)
    }
-}
eval :: (Int, Bool)
eval = (3, True)

f :: a -> b -> a
f a b = a 

result :: (Int, Bool)
result = f eval 5
