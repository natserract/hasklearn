{-# LANGUAGE PostfixOperators #-}

module Basic.Operators where

operators = do
  print equal
  print notEqual
  print greater
  print less
  print greaterThanEqual
  print lessThanEqual
  print negateNum
  print multiply
  print divide
  print divide2
  print multiplyFloat
  print multiplyFloat
  print multiplyFactorize
  print multiplyFactorizeFloat
  print concatStr
  print listsNum
  print listsAlpha
  print logicalNot
  print lambda
  guard True
  print customOp
  print indexOp
  headOp [2, 3, 4]

-- equal
equal = 5 == 5 

-- not equal
notEqual = 4 /= 5 -- True (4 !== 5)

-- Greater/Less
greater = 5 > 5 -- False
less = 4 < 5 -- False
greaterThanEqual = 6 >= 5
lessThanEqual = 4 <= 5 

-- Subsctract/negate operator
negateNum = - 5 + 3 {- -2 -}

-- Multiply/divide operator
multiply = 2 * 2 -- 4
divide = 10 / 2 -- 5.0
divide2 = 10 `quot` 2 -- 5

-- Raise operators
-- **
multiplyFloat = 2 ** 2 -- 4.0 (If ** always return -> Float)
-- ^
multiplyFactorize = 2 ^ 3 -- (2x2x2=8 -> Integer)
-- ^^
multiplyFactorizeFloat = 2 ^^ 3 -- That's mean (2x2x2=8.0 -> Float)

-- Concat operators
concatStr = "Concat" ++ "Str"

-- Range for lists (..)
listsNum = [1..5]
listsAlpha = ['a'..'z']

-- Logical NOT
(!) = not
logicalNot = (True !) -- False

-- Lambda operator 
-- In general this operator used in function
-- Lambda is another name for an anonymous function
-- lambda :: Integer -> Integer
{--
  Haskell: map (\i -> i * 2) [1, 2]
  Javascript: ((v) => v.map(i => i * 2))([1, 2])
--}
lambda = map (\i -> i * 2) [1, 2]

-- Guard
-- In general this operator used for pattern matching
guard :: p -> IO()
guard x
  | True =  print "This value is True"
  | False = print "This value is False"

-- Custom operators
($$) :: [a] -> [a] -> [a]
m1 $$ m2 = m1 ++ m2
customOp = "Alfin" $$ "Surya"

-- Indexing operator
indexOp = [1, 2, 3]!!1 -- find index 1

-- Append head operator / "cons"
headOp :: Show a1 => [a1] -> IO ()
headOp (x:rest) = print rest -- excerpt index 0
