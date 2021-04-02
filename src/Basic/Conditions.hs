{-# LANGUAGE BlockArguments #-}

module Basic.Conditions where

conditions = do
  print (patternM 2)
  print (ifels 2 2)
  print (caseofWBasic 0)
  print (patternAccEl "yoo")
  print (caseofNested 0)

-- # If else
-- (No else if) -> no flexible
ifels :: Eq a => a -> a -> Maybe [Char]
ifels x y =
  if x == y
    then Just "It's same"
    else Nothing

-- # Pattern Matching / guards
patternM :: (Ord a, Num a) => a -> [Char]
patternM s
  | s > 2 = "patternM v = s > 2"
  | s == 0 = "patternM v = it's 0"
  | otherwise = "value not found"

-- pattern matching access elements using where
-- where clause (define local variable)
patternAccEl :: [Char] -> [Char]
patternAccEl s
  | s == "yoo" = "" ++ e
  where
    e = if s == "yoo" then "it's time for break" else "nothing"

-- # Case of
-- case of used in elm (for one rule)
caseofWBasic :: (Eq a, Num a) => a -> [Char]
caseofWBasic b = case b of
  0 -> "caseofWBasic = 0"

-- using do
caseofNested :: (Eq a, Num a) => a -> [Char]
caseofNested v = do
  case v of
    0 -> "case of v = zero"
    1 -> "case of v = one"
    3 -> "case of true"