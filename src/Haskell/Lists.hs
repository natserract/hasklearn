module Haskell.Lists where

-- trace: for debugging
import Debug.Trace (trace)
import Data.Foldable (Foldable(fold))

lists = do
  show ""
  -- show $ pmList [0, 1]
  -- show $ count [2, 3]
  -- basicPatternMatchOfList

-- | List in haskell is homogen -> all values have same type
-- See more: https://www.haskelltutorials.com/guides/haskell-lists-ultimate-guide.html

-- # length
listLength :: Int
listLength = length [1, 2, 3, 4]

-- # reverse
reverseList :: [Integer]
reverseList = reverse [1, 2, 3, 4]

-- # find index
findIndex :: Integer
findIndex = [1, 2, 3, 4, 5] !! 1 -- index 1

-- # range list
rangeList :: [Integer]
rangeList = [10..1000]

-- # filter list
filterList :: [Integer]
filterList = filter (> 2) [1, 2, 3, 4, 5]

-- # basic pattern matching
basicPatternMatchOfList :: [[Char]] -> IO ()
basicPatternMatchOfList [] = error "Lists is empty!" -- Check if nill/empty!
basicPatternMatchOfList ["Infinite"] = print "First index is Infinite" -- Check if length === 0 && "Infinite"
basicPatternMatchOfList (t : rest) = trace "Lists called" print t

-- # pattern matching with aliasing @
aliasListsPatternMatching :: ([Int], [Char]) -> String
aliasListsPatternMatching ([], x) = show 0
aliasListsPatternMatching (p@(t : res), x) 
  | x == "all" = show p -- [1,2]
  | x == "head" = show t -- [1]
  | x == "tail" = show res -- [2]
  | otherwise = show [0]

-- # take 
-- Make a new list containing just the first N elements from an existing list.
takeLists :: [Integer]
takeLists = take 2 [1, 2, 3, 4] -- 1, 2 

-- # minimum/maximum
minimumValueOfLists :: Integer
minimumValueOfLists = minimum [1, 2, 3, 4] -- 1

maximumValueOfLists :: Integer
maximumValueOfLists = maximum [1, 2, 3, 4] -- 4

-- # adding w easyway
addList :: [Integer]
addList = 2:3:4:5:[1] -- [1].push(2..5)

-- # adding list w concat 
-- (++) Add an element to the end of a list / concat.
concatList :: [Integer]
concatList = [1..10] ++ [20..30]

-- # adding list w new_element
-- new_element: Add an element to the start of a list.
tempOfList :: [Integer]
tempOfList = [2, 3, 4, 5]

addNewListInTemp :: [Integer]
addNewListInTemp = 1 : tempOfList

-- # dropping list
dropList :: [Integer]
dropList = drop 1 [1, 2, 3, 4, 5]

-- List of tuples
listOfTuples :: [([Char], Integer)]
listOfTuples = [("John", 20), ("Ben", 21), ("Doe", 30)]

-- Pattern Matching list 2
pmList:: [Int] -> [Int]
-- pmList (_:x) = x -- exclude first element
pmList [x, _] = [r] where  (r:a) = a
pmList list = reverse list

-- count :: [Char] -> [Char]
-- count y = (n:y) where n = n ++ ""

-- oo = count [2, 3]
{--
  let binding expression 
  let used for bind the variables, 
-}

-- listOne = foldl (\)