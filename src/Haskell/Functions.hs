{-# LANGUAGE FlexibleContexts #-}

module Haskell.Functions where

functions = do 
  print apply
  funcMultiplyInf

-- fungsi yang mengambil nilai n argumen dan mengubahnya menjadi n fungsi yang msing2
-- menerima satu argumen

-- # const function
reverseMap :: b -> [a] -> [a]
reverseMap = const reverse 

apply :: [Integer]
apply = reverseMap [1, 2] [3, 4]
  
-- # function to infix
-- | Example: 20 `multiplyInf` 5
multiplyInf :: Num a => a -> a -> a
multiplyInf a b = a * b

funcMultiplyInf :: IO ()
funcMultiplyInf = print (20 `multiplyInf` 2)

clbk :: a -> b -> (a, b)
clbk a b = (a, b)

-- # do: chain all function with same type
applyClbk :: b -> (IO (), b)
applyClbk = do
  clbk $ print ("Clbk applied" , "Only char")