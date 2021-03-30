{-# LANGUAGE PostfixOperators #-}

module Basic.Operators where

import Data.List (sort)

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
  print printFCompose
  print printF2Compose
  print printF3Compose
  print tupl
  print sortDoll
  print sortDollPrior
  whichPrior

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

-- ^ ^

multiplyFactorizeFloat = 2 ^^ 3 -- That's mean (2x2x2=8.0 -> Float)

-- Concat operators
concatStr = "Concat" ++ "Str"

-- Range for lists (..)
listsNum = [1 .. 5]

listsAlpha = ['a' .. 'z']

-- Logical NOT
logicalNot = not True -- False

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
guard :: p -> IO ()
guard x
  | True = print "This value is True"
  | False = print "This value is False"

-- Custom operators
(!) = not

logicalNotCustomOp = (True !)

-- | Concat two strings
($$) :: [a] -> [a] -> [a]
m1 $$ m2 = m1 ++ m2

customOp = "Alfin" $$ "Surya"

-- Indexing operator
indexOp = [1, 2, 3] !! 1 -- find index 1

-- Append head operator / "cons"
headOp :: Show a1 => [a1] -> IO ()
headOp (x : rest) = print rest -- excerpt index 0

-- Tuple constructor
-- (.., ..) "called section, so if you want to write tuple, must use parentheses/()"
tupl :: ([Char], (Integer, Integer))
tupl = ("Tuple", (2, 2))

{--
  Functions compositions (infix operator)
  - . (Compose functions together
  - Is a way to chain two or more functions together.
  - Left/right composition
  - (.) :: (b -> c) -> (a -> b) -> a -> c

  Haskell:
  m x = x * x
  g x = x + 1

  f = m . g
  f2 = g . m

 Javascript:
  function m (x) {
    return x * x
  }

  function g(x) {
    return x + 1
  }

  function f(x) {
    return m(g(2))) -> Output: 9 -> 2+1*3
  }

  function f2(x) {
    return g(m(2)) -> Output: 5 -> 2*2+1
  }

  - operator: => context inheritance from class
--}
m x = x * x

g x = x + 1

c x = x * 2

f = m . g -- m(g(x))

f2 = g . m -- g(m(x))

f3 = (* 2) . m . g -- c(m(g(x)))

printFCompose = f 2

printF2Compose = f2 2

printF3Compose = f3 2

{--
   ($) - dollar,
   - call function without parentheses
   - syntatic sugar
   - $ about priority
   - no arguments be like (.)
   - f $ g $ h x  = f (g (h x))
   - ($) :: (a -> b) -> a -> b
   - See https://typeclasses.com/featured/dollar
--}

sortDoll = sort "alfin" ++ "surya"

sortDollPrior = sort $ "alfin" ++ "surya"

{--
 This means the argument with the $ operator (from the right) which will be evaluated / executed first
--}
whichPrior = print . sort $ "alfin" ++ "surya"
