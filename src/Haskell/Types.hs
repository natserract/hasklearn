
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Haskell.Types where
import Data.Typeable ( typeOf, TypeRep )

types = do 
  print strFunc
  print integerFunc
  print charFunc
  print boolFunc
  print integersFunc
  print checkingTypeFunc
  print $ checkedDivision (2, 1)  
  

-- # Basic Types
strFunc :: String
strFunc = "Str Type"

charFunc :: [Char]
charFunc = "Message" 

boolFunc :: Bool
boolFunc = False 

{-- 
  Haskell has two types for integer numbers: Int vs Integer
  Difference these types is:
    Int:
      -> Int: 32 (min: -2147483648, max: 2147483647) / 64 (min: -9223372036854775808, max: 9223372036854775807) bit, 
      -> Impact on code (more extra code?)
      -> Faster than integer (some case)
      -> Null == 0 (Not Null)
    
    Integer: 
      -> Big Int / Big Num
      -> Hold any number no matter how big, up to the limit of your machine's memory :( 
      -> Impact on app performance
      -> Integer can check/handle null value
--}

intFunc :: Int
intFunc = 1

integerFunc :: Integer
integerFunc = 1

integersFunc :: [Integer]
integersFunc = [2, 3, 4]

-- Haskell also has two types for floating point: Float and Double.
-- Float: f32 (rust), Double: f64 (rust)
floatFunc :: Float
floatFunc = 2.0

doubleFunc :: Double
doubleFunc = 2.0

-- # Type Function
-- Return Type: Monad IO 
-- The return type is the last item
fnType :: Show a => a -> IO ()
fnType = print

-- # Maybe Type
-- Catch the failure/something may not be there
-- Haskell: Maybe<T>: Just(V) / Nothing; Rust: Option<T>: Some(V) / None
checkedDivision :: (Int, Int) -> Maybe Int
checkedDivision (dividend, divisor)
  | divisor == 0 = Nothing
  | otherwise = Just dividend

-- # Generic Type
-- p is universal type
{--
  Haskell:
  -- p: lowercase
  genericFunc :: p -> p
  genericFunc m = m

  TypeScript:
  // P: uppercase 
  function genericFunc<P>(m: P): P {
    return m
  }
-}
genericFunc :: p -> p
genericFunc m = m

applyGenericFunc :: [Char]
applyGenericFunc = genericFunc "Basic Generic Type"

-- # Checking type
checkingTypeFunc :: Data.Typeable.TypeRep
checkingTypeFunc = typeOf "Hello World"

-- # Type Classes
numTypeClasses :: (Num a) => a
numTypeClasses = 100

equalityTypeClasses :: (Eq a, Num a) => (a, b) -> [Char]
equalityTypeClasses (a, b)
    | a == 100 = "True"

-- See more: https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Ord.html
-- >=, >, <, >, <, ==, min, max
ordTypeClasess :: (Ord a) => (Num a) => (a, b) -> Bool
ordTypeClasess (x, y) = x >= 10

-- See more https://hackage.haskell.org/package/base-4.8.2.0/docs/src/GHC.Enum.html#succ
enumTypeClasses :: (Enum a) => a -> a
enumTypeClasses = succ

numWEnumTypeClasses :: (Num a, Enum a) => [a]
numWEnumTypeClasses = [1..100]

floatTypeClasses :: (Fractional a)  => a
floatTypeClasses = 50.2

boundedTypeClasses :: (Bounded a) => a
boundedTypeClasses = minBound

-- Show: display type / parameter converted to an string output 
showTypeClasses :: (Show a) => a -> String
showTypeClasses = show

-- # forall (existential types)
-- This is parametric polymorphism (parameter <T>: can work on any type)
-- message :: forall a b. (b -> b) -> (a -> b) -> Maybe a -> b
{--
  checkedDivision :: (Int, Int) -> Maybe Int
  checkedDivision (dividend, divisor)
    | divisor == 0 = Nothing
    | otherwise = Just dividend
--}
-- -> forall steps

-- b as parameter, return is function itself
genSingleParam :: forall b. b -> b
genSingleParam = genSingleParam

applyGenSingleParam :: Integer
applyGenSingleParam = genSingleParam 2 -- generic type -> Integer

-- x, y as parameter is unknown type -> return is boolean
genDoubleParam:: forall x y. x -> y -> Bool
genDoubleParam x y = True

applyGenDoubleParam :: Bool
applyGenDoubleParam = genDoubleParam 2 3

-- x, y as parameter, return type is Maybe<T>
{-- 
  (Eq x, Eq y) => (Num x, Num y) => (x, y) these means, 
  
  (dividend: x, divisor: y), 
  genericCheckedDivision` there is an operation (x == 0, y == 0)
  In Haskell it to do that using typeclasses
    -> Eq: The Eq class defines equality (==) and inequality (/=).
    -> Num: Numeric type classes (0, 1, 2, ...)

  Then the next step is to wrap parameters => (x, y) as a (dividend, divisor)
--} 

-- genericCheckedDivision (2, 0)
genericCheckedDivision:: forall x y. (Eq x, Eq y) => (Num x, Num y) => (x, y) -> Maybe x
genericCheckedDivision (dividend, divisor)
  | divisor == 0 || dividend == 0 = Nothing
  | otherwise = Just dividend