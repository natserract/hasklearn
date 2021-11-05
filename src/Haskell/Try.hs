{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
-- {-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Haskell.Try where
import Data.Typeable (typeOf)
import Data.Monoid

tryRoot = do
  print ""
  print implLet
  print implLeftO
  print cool
  print $ take 5 fibs
  -- print $ strict' [1, 2]
  print $ j [1, 2]
  print "laziness"


tryCase :: IO ()
tryCase = do
  print ""

data List t = Nil
data Poly

fno :: forall a. a -> Poly
fno = fno

-- How to read Haskell

(<→>) :: ∀ to₁ to₂. (to₁ -> to₂) -> to₁ -> to₂
(<→>) f = f

fn :: String
fn = show <→> "Hello World!"

{--

Btw, ternyata haskell juga support math symbol syntax seperti di agda
```hs
{-# LANGUAGE UnicodeSyntax #-}
(<→>) :: ∀ to₁ to₂. (to₁ -> to₂) -> to₁ -> to₂
(<→>) f = f
```

Reference: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/unicode_syntax.html#
-}


-- :: a -> b -> c
-- :: fa(fb(a)) = fa(fb(a) -> rb) -> fa(rb) -> ra
(<.>) ::
  (b -> c)
  -> (a -> b)
  -> a -> c
(<.>) fa fb a = fa (fb a)

-- implG = print <.> mconcat <$$> ["Red, Blu"]

-- fa(c)

-- Step 1: (a -> c) 
-- Step 2: (b -> c) 
-- c  = Either a b -> c
type Result a b = Either a b

f ::
  (a -> c)
  -> (b -> c)
  -> Result a b
  -> c
-- Pattern Matching and Branching
f fn _ (Left x) = fn x
f _ fn (Right y) = fn y

-- f fa fb c = do

--   fa $ case c of 
--     Left m0 -> m0 where m_ = m0
--     Right _ -> m $ case c of 

-- implS = f length (\x -> 2) eith

-- PLAY WITH NOTATION

-- MONOID
-- (<>) :: Semigroup a => a -> a -> a
-- infixr 6 <>
implConcatList = "alfin" <> "surya"

-- Multiple Do (>>)
implDo = (\_ -> 2 * 2) >> (\_ -> 5 * 2)

moo =  "red" >>= show -- It's reverse, but it's more usefull


-- Left operator (<-) for IO Operation
implLeftO = do
  x <- [1..4]
  y <-  [5..10]
  show $ y * x
  -- putStrLn str



-- ASsign / local variable
implLet = do
  (a, b) where (a, b) = (2, 10) 

-- mol = implLeftOp

impLeftO2 :: (Monad m, Num t, Num (m b)) => t -> m b
impLeftO2  = do
  x <- impLeftO2
  return x

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0  _  = []
take' n (x:xs) = x : take' (n - 1) xs

recList :: ∀ a. a -> [a]
recList n = n : recList n -- n:(n:(n:(....)))

j :: [a] -> [a]
j [] = []
j a@(x:xs) = x:x:x:xs 

go = 2 : [2, 3]

cool = take' 5 $ recList 1
{-
  take' 5 (recList 1)
  take' 5 (1 : recList 1)

  1 : take' 4 (recList 1) --- 5 - 1
  1 : take' 4 (1 : recList 1) --- 5 - 1

  1 : 1 : take' 3 (recList 1)
  1 : 1 : take' 3 (1 : recList 1)
  ...
-}

strict' !xs = xs

match :: Int -> Int
match 0 = 1
match 1 = 1
match n = n 

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]

-- impl = fst $ f2 (Just 2)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{--

<!-- 

https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form 

WHNF: nilai yg ekspresinya sudah di evaluasi, tetapi sub ekspresinya blm. Contoh: \x -> 2 + 2 , y nya tidak dievaluasi

NF : 42, sepenuhnya sudah dievaluasi dan tidak ada sub-ekspresi yang dapat dievaluasi 
https://www.seas.upenn.edu/~cis194/spring13/lectures/06-laziness.html
https://haskell.mooc.fi/part2
-->
--}