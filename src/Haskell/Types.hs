{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Haskell.Types where

import Data.Typeable (TypeRep, typeOf)

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

-- # parameter as a function
-- ( ) type is a function
clbk :: a -> b -> (a, b)
clbk a b = (a, b)

applyClbk :: b -> ((p -> IO (), [Char]), b)
applyClbk = clbk (\c -> print "Clbk applied" , "Only char")

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

-- Either type
-- is a enum type with 2 options, like Result<T, M> in Rust
isEither :: Bool -> Either String Int
isEither a
  | a = Left ""
  | not a = Right 2

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
numWEnumTypeClasses = [1 .. 100]

floatTypeClasses :: (Fractional a) => a
floatTypeClasses = 50.2

boundedTypeClasses :: (Bounded a) => a
boundedTypeClasses = minBound

-- Show: display type / parameter converted to an string output
showTypeClasses :: (Show a) => a -> String
showTypeClasses = show

-- # forall (existential types)
-- This is parametric polymorphism (parameter <T>: can work on any type)
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
genDoubleParam :: forall x y. x -> y -> Bool
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
genericCheckedDivision :: forall x y. (Eq x, Eq y) => (Num x, Num y) => (x, y) -> Maybe x
genericCheckedDivision (dividend, divisor)
  | divisor == 0 || dividend == 0 = Nothing
  | otherwise = Just dividend

-- # Type: type synonyms
-- Can't use union, enum
-- It's same : a -> Int
type Constraint t = String -> t

applyType :: Constraint Int
applyType a0 = 100

-- # newtype
-- allow for deriving
-- if you have one constructor just use newtype, if multiple use data
-- custom type
newtype Primitive t = Primitive t

-- # Algebraic Data Types (ADT)
-- Union / Enum type
{--
  Haskell:
    data PrimitiveOrConstructor t = Constructor t | String | Number | Boolean

  TypeScript:
    type PrimitiveOrConstructor<T> =
      | Constructor<T>
      | 'string'
      | 'number'
      | 'boolean';
--}
-- basic adt
data Color = Red | Blue

checkColor :: Color -> IO ()
checkColor c = case c of
  Red -> print "red"
  Blue -> print "blue"

-- adt record
-- see: https://www.schoolofhaskell.com/user/Geraldus/algebraic-data-types-adts-with-aeson
data Field = Field
  { idx :: Int,
    names :: String,
    phone :: Int,
    address :: String
  }

checkNum :: p -> Field
checkNum n =
  Field
    { idx = 2,
      names = "",
      phone = 628191069231,
      address = "Jl. Raya Kartini 99"
    }

-- generic adt (parameterized data type)
data Days = Monday | Tuesday
data TimeOptions t = Day !t | Week !t -- !t -> strict typpe

checkTime :: TimeOptions Days -> IO ()
checkTime t = case t of
  (Day d) -> case d of
    Monday -> print "Monday"
    Tuesday -> print "Tuesday"

-- adt w deriving
-- deriving: derived class type -> for allowing to convert any type (eg. Show, Eq, Ord, etc)
data IsNotValid = Valid | NotValid deriving (Show) 

isNotValid :: IsNotValid -> IO ()
isNotValid x = print Valid -- for allowing show in Print use (Show)

-- TODO: ADT Tree