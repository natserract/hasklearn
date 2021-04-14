{-# LANGUAGE FlexibleContexts #-}

module Haskell.Functions where

functions = do
  funcMultiplyInf

-- # function with multiple arguments
multipleArgs :: Num a => a -> a -> a -> a
multipleArgs x y z = x * y * z

-- Lambda/Anonymous function
-- anonym 2 5 = 10
anonym :: Integer -> Integer -> Integer
anonym = \x y -> x * y

-- # Currying function 
-- const : is a unary function. Remember curried functions always return a unary function.
-- See this reference: https://github.com/wisn/jargon-pemrograman-fungsional#currying

-- Currying: Function that returns another function that takes (one parameter) at a time.
-- Partial: function returning another function that might return another function, 
-- but each returned function can take several parameters.
-- reverseMap [3, 2, 1]
reverseMap :: b -> [a] -> [a]
reverseMap = const reverse 

-- # Partial function
-- In computer science, partial application (or partial function application) refers to the process of 
-- fixing a number of arguments to a function, producing another function of smaller arity.
-- In Haskell, all functions are curried

-- Must resolve thenPartial 5
-- f partial = x -> y -> result
-- f partial 2 = y -> result
-- f partial 2 5 = result
partial :: Integer -> Integer -> Integer -- a0 -> (a1 -> result)
partial = \x -> \y -> x + y 

thenPartial :: Integer -> Integer 
thenPartial = partial 2 -- Only fixing x argument, and leaving/return a function with one more argument

finalPartial :: Integer
finalPartial = partial 2 3 -- get the result, because x y resolved

-- # function to infix
-- | Example: 20 `multiplyInf` 5
multiplyInf :: Num a => a -> a -> a
multiplyInf a b = a * b

funcMultiplyInf :: IO ()
funcMultiplyInf = print (20 `multiplyInf` 2)

-- # Function composition
{--
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
m :: Num a => a -> a
m x = x * x

g :: Num a => a -> a
g x = x + 1

c :: Num a => a -> a
c x = x * 2

f :: Integer -> Integer
f = m . g -- m(g(x))

f2 :: Integer -> Integer
f2 = g . m -- g(m(x))

f3 :: Integer -> Integer
f3 = (* 2) . m . g -- c(m(g(x)))

-- Todo:
-- Higher Order Functions
