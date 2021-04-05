{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PostfixOperators #-} -- Syntax Extension

module Haskell.Infix where

infixOp = do 
  print ""

-- Infix operation
-- Many people says, prefer to use infix syntax rather than prefix for binary operation
-- Because make expressions and statement of mathematical properties more readable
{--
  Parameter:
  infixr: the operator is right-associative
  infixl: the operator is left-associative
  infix: the operator is non-associative
--}

-- (.*) :: Num a => a -> a -> a
-- (.*) = error ""
-- (.+) :: [a] -> [a] -> [a]
-- [] .+ xs = xs
-- (x : xs) .+ ys = x : (xs .+ ys)

-- Const only return first argument (a)
-- const :: a -> (b -> a) / const x y = \_ -> x
-- const x is a (unary function) that evaluates to x for all inputs.
-- Only two arguments, return default value
-- Function return array if argumen is true
-- const(x)
-- ((+) `foldl` 0) [1..6]
{--
<!-- 
  Assosiatif (Pengurutan)
  - Meskipun pengurutan berbeda, hasil yg dihasilkan sama
  10 + 5 + 5 = <a0> - <a1> + <a2> / Left associative (default)
  10 + (5 + 5) = <a0> - <a1> / Right associative
  
  See: https://en.wikipedia.org/wiki/Associative_property#Non-associative_operation
  Tidak peduli pada urutan
  Tidak memenuhi hukum assosiatif, nilai yang dihasilkan jika urutannya berbeda hasilnya tidak sama
  (5 - 3) - 2 = Non associative
 -->
--}

-- g = ((+) `foldl` 0) [1..6]
(!) :: Bool -> Bool
(!) = not

g = (True !)