{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- See: https://www.haskell.org/onlinereport/basic.html

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
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
  printable
  print ("httpAction", httpAction)


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

-- Either type
-- is a enum type with 2 options, like Result<T, M> in Rust
isEither :: Bool -> Either String Int
isEither a
  | a = Left ""
  | not a = Right 2

-- # Type Classes
-- What is typeclasses? Typeclasses define a set of functions that can have different  
-- implementations depending on the type of data they are given.
-- Check types of instance (e.g Eq: (==) :: a -> a -> Bool), using stack ghci ':info Eq'
numTypeClasses :: (Num a) => a
numTypeClasses = 100

equalityTypeClasses :: (Eq a, Num a) => (a, b) -> [Char]
equalityTypeClasses (a, b)
  | a == 100 = "True"

-- Own Type Classes
-- I called it's implementation of types (impl of struct / types) like Rust
-- class in haskell it's not class in another language as object. It's different.

-- Basic type classes
class Printable a where
    fmt :: a -> IO ()
instance Printable String where
  fmt = print

printable :: IO (); 
printable = fmt "Hallo fmt"

-- Type families

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
    data PrimitiveOrConstructor t = Constructor t 
      | String 
      | Number 
      | Boolean

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
-- Records type
-- see: https://www.schoolofhaskell.com/user/Geraldus/algebraic-data-types-adts-with-aeson
data Field = Field
  { idx :: Int,
    names :: String,
    phone :: Int,
    address :: String
  }

checkNum :: Field
checkNum = do {
  Field
    { idx = 2,
      names = "",
      phone = 628191069231,
      address = "Kartini Street No. 99"
    }
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
-- deriving (Show, Eq): instance of type class
data IsNotValid = Valid | NotValid deriving (Show) 

isNotValid :: IsNotValid -> IO ()
isNotValid x = print Valid -- for allowing show in Print use (Show)


-- Todo:
-- Fun deps
-- Types Family
-- ADT Node Tree
-- Pattern Synonyms

-- # Fun deps
-- | Functional dependencies are used to constrain the parameters of type classes
-- a b -> c is a functional dependency, a and b are determined uniquely
-- c sepenuhnya ditentukan oleh a dan b
-- Avoiding ambigous types, because for default a, b, c is independent
class Result a b c | a b -> c where
  instancex :: a -> b -> c

--  a, b is uniquely
-- instance Result Bool Int String where check = check  -- //error
-- instance Result Bool Int Char where check = check
instance Result String Int String where instancex = instancex
instance Result Bool Int Char where instancex = instancex

class Calc a b | b -> a where
  check :: a -> b -> Bool
instance Eq a => Calc a a where
  check a b = a == b -- check :: forall a b. Calc a b => a -> b -> Bool

--  If without fundeps, you will see this errors:
-- • Ambiguous type variables ‘a0’, ‘b0’ arising from ...
--  Probable fix: use a type annotation to specify what ‘a0’, 'b0' ...
f0 :: Bool
f0 = check 2 2

-- Multiple fundeps
class Mult a b c | a b -> c, b -> a where
  mult :: a -> b -> c -> a
instance Num a => Mult a a a where
  mult a b c = a * b * c

m1 :: String
m1 = show $ mult 2 10 2


-- # Types families
-- type families vs fundeps: https://gitlab.haskell.org/ghc/ghc/-/wikis/tf-vs-fd
-- It's like conditional types in TypeScript: "T extends U ? X : Y"
-- G1 & G2 it's same, 2 syntax version
-- Type families come in three variations: data families, open type synonym families, closed type synonym families
-- open type synonym families
type family G a :: *
type instance G Int = Bool
type instance G String = Char

-- closed type synonym families
type family G1 a where
  G1 Int = Bool
  G1 a = a -- Otherwise Int

-- data families (type)
-- Class
data family A t
data instance A Int = Bool
data instance A String = Char

-- data families (function)
data family A2 a b
data instance A2 a b where
  DoSomeThing :: A2 a Int
  DoSomeThingElse :: A2 a Bool

f :: G1 Int
f = True

-- Real case
-- Using type classes
data Http = GET 
  | POST 
  | PUT 
  | DELETE 
  | PATCH deriving (Eq, Enum)

data Endpoint = Tnc 
  | Register 
  | EmailVerif 
  | NotFound deriving (Eq, Enum, Show)

class HttpMethod u h e | h -> e 
  where selectMethod :: u -> h -> e -- (Maybe e?)
instance HttpMethod [Char] Http Endpoint where
  selectMethod u h = do
    case h of
      GET -> Tnc
      POST -> Register
      PUT -> Register
      DELETE -> NotFound
      PATCH -> NotFound

httpAction :: Endpoint
httpAction = do
  let url = "url::endpoint"
  selectMethod url GET -- Output:: Tnc
