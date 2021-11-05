{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE IncoherentInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE DeriveAnyClass #-}

-- 
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE IncoherentInstances #-}

module Haskell.GenericTypes where

import Data.Typeable (TypeRep, typeOf)

genTypes = do
  print a
  print "type ()"
  print impl
  print impl2
  print az
  print " "
  print "Out"
  -- print f2

-- print d10
-- print d92

-- GENERIC VARIABLES
-- Default:
-- a :: Integer
-- a = 2

-- It's not allowed :: No instance for (Num *) arising from the literal ‘2’
-- a :: forall b. b
-- a = 2

-- It's allowed but, should defined classes, gmna klo generic?
a :: Integer
a = 2

-- data family GenericVariable
-- data instance GenericVariable = Constructor Char
-- data GenericVariable = Constructor [Char]
-- newtype T = T [Char]

-- Synonym dari Integer <-> Bukan buat data baru
-- type family T

-- type instance T = Integer

-- Jika mau buat instance, harus define family dl: indexed type family
-- type instance T = String If multiple get error: Conflicting family instance declarations:

-- a2 :: T
-- a2 = 2

-- Generic
-- a is type parameter
-- type family V a where
--   V a = a

-- a3 :: V String
-- a3 = ""

-- data family N :: * -> *

-- data instance N a where
--   Error :: N a

-- n3 :: N String
-- n3 = Error --from * -> * | to: String -> String

-- data instance N a where

-- Expected a type, but ‘Num’ has kind ‘* -> Constraint’
-- https://wg-romank.github.io/peano/
-- https://github.com/dmjio/compiler-in-haskell/search?q=PExpr

--
-- class Calc a b | b -> a where
--   check :: a -> b -> Bool
-- instance Eq a => Calc a a where
--   check a b = a == b -- check :: forall a b. Calc a b => a -> b -> Bool

--  If without fundeps, you will see this errors:
-- • Ambiguous type variables ‘a0’, ‘b0’ arising from ...
--  Probable fix: use a type annotation to specify what ‘a0’, 'b0' ...
-- f0 :: Bool
-- f0 = check 2 2

hyp = 2

hyp :: Integer

n = 10 + 5

f :: Num a => a -> a
f x = x * 2

hyl = 250

hyl :: Integer
-- do:: Argument -> Return
tsWArg :: Integer -> Integer
tsWArg n = n * 0

-- do:: Argument -> Return
d0_sec :: Integer -> Integer -> Integer
d0_sec x y = x + y

_tsArgP :: Integer -> (Integer -> Integer) -- a0 -> fungsi (a1 -> a2) returnnya adalah a2
_tsArgP x y = x + y

str :: [Char] -> [Char]
str s = s

az = _tsArgP 10 $ 5 + 5 -- _tsArgP 10 (5 + 5)

out = _tsArgP 10

amore = _tsArgP 10 6

az2 = d0_sec 10 5 + 5

tmb :: Integer -> Integer -> Integer
tmb x y = x + y

az3 = _tsArgP 10

-- test :: Integer
test = 10 + (5 + 5)

-- 2 * 5 + 6 = 16
-- 2 * (5 + 6) = 22

-- Two argument
d1 :: Int -> Int -> Int
d1 x y = (+) x y

--tanda kurung berfungsi untuk mengelompokkan bagian-bagian a
-- --> (tipe) ekspresi yang akan dikelompokkan secara berbeda berdasarkan prioritas dan/atau
-- --> asosiatifitas operator, mis

d2_1 x y = x * y

-- One argument
d2 :: Int -> Int -> (Int -> Int -> Int) -> Int
d2 x y fn = fn x y

{-
  https://mail.haskell.org/pipermail/beginners/2012-November/010999.html

  Jadi artinya adalah:

  JS:
    function d2 (x: number, y: number, fn: (n1: number, n2: number) => number): number {
      return fn(x, y)
    }

    function count(x: number, y: number): number {
        return x * y
    }

    d2(2, 3, count)
-}

d3 = d2 2 3 (\x y -> x * y)

-- output: 6
-- d3 = d2 2 3 d2_1

-- d2 :: (Int -> Int) -> Int
-- d2 x y = x + y

-- d1 :: Int -> (Int -> Int)
-- d1 x = 20 + 2
-- Integer -> (Integer -> Integer)
tsArgP :: (Integer -> Integer) -> Integer
tsArgP fn = fn 5

tsArgPHave :: Integer -> (Integer -> Integer) -> Integer
tsArgPHave x fn = fn x * x

tsArgPHave2 :: (Integer -> Integer) -> Integer -> Integer
tsArgPHave2 fn x = fn x * x

-- tsArgPHave2 x fn = fn x

m = 2 * 2

impl = tsArgPHave 2 (\x -> 2 + 2)

impl2 = tsArgPHave2 (+ 2) 2

-- imp3 = tsArgP 2 5
-- Lihat, `Integer -> Integer -> Integer` ini terdiri dari 2 argument dan 1 Return

-- (3)
-- a0 -> a1
__tsArgP :: (Integer -> Integer) -> Integer
__tsArgP fn = fn 5

{-
Impl of polimorfisme
  Polimorfisme ada 2:
  - polimorfisme ad-hoc (overloading): fungsi yang sama dengan perilaku yang berbeda untuk tipe yang berbeda (melalui kelas tipe Haskell)
  - fungsi yang sama dengan perilaku yang sama untuk tipe yang berbeda (melalui fungsi berparameter tipe. In prinsipnya, tipe tidak masalah, tetapi kami menggunakan kelas tipe untuk membatasi jenis yang dapat diterima).
  - Tipe data yang tidak didefiniskan, jadi bisa menggunakan tipe apa saja its like generic

  Monomorfik:
  - Tipe didefinisikan : String -> String

  Deriving:
  https://kowainik.github.io/posts/deriving
  https://www.inf.ed.ac.uk/teaching/courses/inf1/fp/lectures/2017/lect13.pdf
-}

-- (existential types)
-- This is parametric polymorphism (parameter <T>: can work on any type)
-- a, b as parameter is unknown type -> return is boolean
-- secara eksplisit menentukan kuantifikasi universal dalam tanda tangan tipe polimorfik
-- Di Haskell 98, semua tanda tangan tipe secara implisit dikuantifikasi secara universal di tingkat luar, misalnya

--(1)
g :: b -> b
g = g

--(2) itsame with 1
g2 :: forall b. (b -> b)
g2 = g2

-- * : types of types (tipe dari tipe)

-- pol :: (b ~ b) -> Bool
-- pol x = d

-- implPol :: *
-- implPol = pol 2

-- ~ persamaan type
-- c :: a ~ a -> b ~ b

fg :: forall a. (a -> a)
fg = fg

-- am = show implGs0

-- implGs1 = gs True

-- implGs2 = gs 3.2

-- implGs4 = gs [1..100]

-- Implement generics
-- function notUndefined<T>(x: T | undefined): x is T {
--   if (x === undefined) {
--     return false
--   }
--   return true
-- }

-- fo:: forall t. t -> t -> t -- default -- ERR: impl22 x y = x * y
-- fo x y = x * y
-- type T2 = forall t. (Num t => t) -> (Num t => t) -> Num t => t -- Ok: impl22 x y = x * y

-- impl22 :: T2
-- impl22 x y = x

-- goImpl = impl22 2 2
goImpl = 2

-- Overloading:
-- Error: Duplicate type signature
fn :: forall a. (Num a => a) -> (Num a => a) -> Num a => a
fn x y = x * y

-- * :: forall a. Num a => a -> a -> a

-- fn :: forall t. (Eq t => t) -> (Eq t => t) -> Eq t => t

type Generic = forall x y. (Eq x => x) -> Eq x => x -> Maybe Bool

(~/) :: Generic
a0 ~/ a1 = do
  if (==) a0 a1
    then Just True
    else Nothing

type ReadT a = String -> [(a, String)]

type ReadD a = String -> [(a, String)]

m23 :: ReadD Bool
m23 a = [(True, "")]

implCase = 2 `fn` 2 -- Compare 2 integer, to Maybe type

-- genericCheckedDivision :: forall x y. (Eq x, Eq y) => (Num x, Num y) => (x, y) -> Maybe x
-- genericCheckedDivision (dividend, divisor)
--   | divisor == 0 = Nothing
--   | otherwise = Just dividend
data Null = Null

data Err = Err
  { name :: String,
    message :: String,
    stack :: String
  }

class ExtractError e where
  extractError :: e -> e

-- instance ExtractError a where
--   extractError a = a

-- instance ExtractError (Maybe [Char]) where
--     extractError a = a

-- instance ExtractError (Maybe Err) where
--       extractError a = a

{--
  Fix: (1)
  add : 
  {-# LANGUAGE UndecidableInstances #-}
-}

class Compare1

class Compare2 a where
  (//) :: a -> a -> Bool

instance Compare2 a

instance {-# INCOHERENT #-} Compare2 Int where
  (//) a b = a == b


fnZero  = (//) 2 2
newtype NewOrder a = NewOrder a

class Compare a where
  type Instance c a

  (=/=) :: a -> a -> Bool

instance (Eq a) => Compare a where
  type Instance Int a = Int
  type Instance Bool a = Bool

  (=/=) a b
    | a == b = True
    | otherwise = False

class Compare a => Order a where
  (/=>) :: a -> a -> Bool

instance (Ord a, Show a) => Order a where
  (/=>) x y
    | x > y = True
    | otherwise = False


-- {-# LANGUAGE TypeFamilies #-}
{-
Polymorphic use
> poly :: [a] -> Int
> poly = count
is problematic:
    Overlapping instances for Count [a] arising from a use of ‘count’
    Matching instances:
      instance [overlappable] Count [a] -- Defined at overlap.hs:13:31
      instance [overlapping] Count [Char] -- Defined at overlap.hs:17:30
-}

-------------------------------------------------------------------------------
-- Incoherent instances
-------------------------------------------------------------------------------

class Collects a where
  type Elem a  -- associated type synonym
  insertS :: Elem a -> a

instance (Collects a) => Collects [a] where
    type Elem [a] = a
    insertS = insertS

instance (Collects Int) => Collects Int where
    type Elem Int = String
    insertS = insertS

-- instance {-# INCOHERENT #-} (Collects String) => Collects String where
--     type Elem String = Char
--     insertS = insertS

-- instance {-# INCOHERENT #-} (Collects String) => Collects String where
--   type Elem Int String = String
--   insertS a = a

f2 = (/=>) 4.2 4.2

-- Dengan adanya ini kita bisa mengubah, 
f10 :: Int -> [Int]
f10 = insertS @[Int]


f11 :: String -> Int
f11 = insertS @Int


--  (#==) :: a -> a -> Bool

-- class (TypeOf a) => (Compared a) where
--   (/>) :: a -> a -> Bool

-- instance TypeOf Int where
--   isSameValue a b = ""
-- deriving  instance (Show a, Eq a, Ord a, Num a) => TypeOf a 
  --{-# LANGUAGE DeriveAnyClass #-}

-- Num a: * -> Constraint
-- Typeof:: * -> * -> Constraint


-- instance (TypeOf a) => (TypeOf a) where-- (A) kepala instance
  -- (#=) a b = show $ a == b

-- f2 = (#=) 5 2


-- (#=) a b = show $ a == b
-- (#==) a b
--   | a > b = True
--   | otherwise = False

-- AMBIGOUS
-- instance {-# OVERLAPPABLE #-} (TypeOf Int, Ord Int, Num Int) => TypeOf Int where  -- (A) kepala instance
--   (#=) a b = show $ a == b
--   (>==) a b = a > b

-- instance {-# OVERLAPPABLE #-} (TypeOf [Char], Ord [Char]) => TypeOf [Char] where  -- (A) kepala instance
--   (#=) a b = show $ a == b
--   (>==) _ _ = False

-- If haven't No instance nor default method for class operation
-- instance TypeOf String => TypeOf String where  -- (B) next instance
--   (#=) a b = show $ a == b

-- instance (TypeOf a, Eq a) => (Compared a) where
--   x /> y = x == y


-- instance Print (G String) where
--   typeOf a = a
--   typeOf a = show a

-- instance Num Integer => Print Integer where
--    typeOf a = show a

-- f2 :: (Print (a -> Int), Num (a -> Int)) => a -> Int == 2
-- f2 :: (Print [Char]) => [Char] == ""
-- f2 :: (Print Int) => Int
-- f3 = typeOf "True"

-- getExceptionFromError = extractError Left 2

-- getExceptionFromError2 = extractError (Just Err {
--   name = "",
--   message = "",
--   stack = ""
-- })

data Color = Red | Blue deriving (Eq)

operation1 :: Color -> IO ()
operation1 c = case c of
  Red -> print "red"
  Blue -> print "blue"

operation2 =  Red
operation3 =  Red /= Blue -- No instance for (Eq Color) arising from a use of ‘/=’
