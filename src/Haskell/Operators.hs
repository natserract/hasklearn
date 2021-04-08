{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE BangPatterns #-}

module Haskell.Operators where
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
  print $ "ReadAs Operator " ++ readaslists ([1, 2], "all")
  print (bang undefined) -- Okay
  print (recurseLists [1, 3])
  print (recurseStr ["recurse", "str"])
  
-- # equal
equal :: Bool
equal = 5 == 5

-- # not equal
notEqual :: Bool
notEqual = 4 /= 5 -- True (4 !== 5)

-- # Greater/Less
greater :: Bool
greater = 5 > 5 -- False

less :: Bool
less = 4 < 5 -- False

greaterThanEqual :: Bool
greaterThanEqual = 6 >= 5

lessThanEqual :: Bool
lessThanEqual = 4 <= 5

-- # Subsctract/negate operator
negateNum :: Integer
negateNum = - 5 + 3 {- -2 -}

-- # Multiply/divide operator
multiply :: Integer
multiply = 2 * 2 -- 4

divide :: Double
divide = 10 / 2 -- 5.0

-- # Multiply using infix operator `quot`
divide2 :: Integer
divide2 = 10 `quot` 2 -- 5

-- # Raise operators

-- **

multiplyFloat :: Double
multiplyFloat = 2 ** 2 -- 4.0 (If ** always return -> Double)

-- ^

multiplyFactorize :: Integer
multiplyFactorize = 2 ^ 3 -- (2x2x2=8 -> Integer)

-- ^ ^

multiplyFactorizeFloat :: Double
multiplyFactorizeFloat = 2 ^^ 3 -- That's mean (2x2x2=8.0 -> Double)

-- # Concat operators
concatStr :: [Char]
concatStr = "Concat" ++ "Str"

-- # Range for lists (..)
listsNum :: [Integer]
listsNum = [1 .. 5]

listsAlpha :: [Char]
listsAlpha = ['a' .. 'z']

-- # Logical NOT
logicalNot :: Bool
logicalNot = not True -- False

-- # Lambda operator
-- In general this operator used in function
-- Lambda is another name for an anonymous function
-- lambda :: Integer -> Integer
{--
  Haskell: map (\i -> i * 2) [1, 2]
  Javascript: ((v) => v.map(i => i * 2))([1, 2])
--}
lambda :: [Integer]
lambda = map (\i -> i * 2) [1, 2]

-- # Guard
-- In general this operator used for pattern matching
guard :: p -> IO ()
guard x
  | True = print "This value is True"
  | False = print "This value is False"

-- # Indexing operator
indexOp :: Integer
indexOp = [1, 2, 3] !! 1 -- find index 1

-- # Append head operator / "cons"
headOp :: Show a1 => [a1] -> IO ()
headOp (x : rest) = print rest -- excerpt index 0

-- # Tuple constructor
-- (.., ..) "called section, so if you want to write tuple, must use parentheses/()"
tupl :: ([Char], (Integer, Integer))
tupl = ("Tuple", (2, 2))

{--
  # Functions compositions (infix operator)
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

printFCompose :: Integer
printFCompose = f 2

printF2Compose :: Integer
printF2Compose = f2 2

printF3Compose :: Integer
printF3Compose = f3 2

{--
   # ($) - dollar,
   - avoid parentheses (call)
   - syntatic sugar
   - $ about priority
   - no arguments be like (.)
   - f $ g $ h x  = f (g (h x))
   - ($) :: (a -> b) -> a -> b
   - See https://typeclasses.com/featured/dollar
--}

sortDoll :: [Char]
sortDoll = sort "alfin" ++ "surya"

sortDollPrior :: [Char]
sortDollPrior = sort $ "alfin" ++ "surya"

{--
 This means the argument with the $ operator (from the right)
 which will be evaluated / executed first
--}
whichPrior :: IO ()
whichPrior = print . sort $ "alfin" ++ "surya"

-- # Custom operators
(!) :: Bool -> Bool
(!) = not

logicalNotCustomOp :: Bool
logicalNotCustomOp = (True !)

-- | Concat two strings
($$) :: [a] -> [a] -> [a]
m1 $$ m2 = m1 ++ m2

customOp :: [Char]
customOp = "Alfin" $$ "Surya"

-- # “read as” operator / as-pattern (aliasing)
-- @ -> Decompose all parameter
readaslists :: ([Int], [Char]) -> String
readaslists ([], x) = show 0
readaslists (p@(t : res), x) 
  | x == "all" = show p -- [1,2]
  | x == "head" = show t -- [1]
  | x == "tail" = show res -- [2]
  | otherwise = show [0]


-- # bang operator
-- ! -> force evaluation (strictness flag)
-- ! -> Look like strictNullChecks in TypeScript
-- write strict variable/tuple
-- undefined is exceptional value
bang :: p -> Bool
bang x = True -- x undefined (when compile no errors)

-- Prelude.undefined
bangStrict :: p -> Bool 
bangStrict !x = True -- x undefined (errors because x evaluated strictly | no undefined)

{--
# list comprehension generator
<- = generator -> binding element of list
Use for: determination of value
Use for: manipulate lists
recursive function
(|) is a pipe generator

Haskell:
recurseLists :: Num a => [a] -> [a]
recurseLists arr = [arr * 2 | arr <- reverse arr]

Javascript:
function recurseLists(arr){
  let result = []
  
  for(let i=0; i<arr.length; i++){
	  let element = arr[i]
    
    if (typeof element !== "number") result = [0] //base case
      recurseLists(result.push(element * 2)) // recursive
  }
  
  return result.reverse()
}

recurseLists(
   let result = []
  	
   loop < arr.length (2) (
      recurseLists(
        result.push(el*2) // [2, 4] -> index 1

          recurseLists(
              result.push(el*2) // [2] -> index 0
          )    
      )  
   )
   
   result.reverse() = [4, 2] -> combination of ([el*2], [el*2])
)    
**/
--}

-- <- operator use for stored value
recurseLists :: Num a => [a] -> [a]
recurseLists arr = [arr * 2 | arr <- reverse arr]

-- I think it's like (where operation) 
-- in this example, t bind into chars
-- <- operator use for stored value
recurseStr :: [a] -> ([a], [Char])
recurseStr chars 
  | t <- "it's work!" = (reverse chars, t) -- (["str","recurse"],"it's work!")

concatTwoStr :: [Char] -> [Char]
concatTwoStr str = do 
    g <- ["Ambisious"] -- <- operator use for stored value
    if str == ""
      then g ++ ""
    else str ++ ""