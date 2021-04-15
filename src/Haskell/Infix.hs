{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PostfixOperators #-} -- Syntax Extension

module Haskell.Infix where
import Prelude hiding ((/=))

infixOp = do 
  print ("infixr", rightAssociative)
  print ("infixl", leftAssociative)
  print ("infix", nonAssociative)

-- Infix operation (Custom)
-- Many people says, prefer to use infix syntax rather than prefix for binary operation
-- Because make expressions and statement of mathematical properties more readable
-- See my article: https://natserract.vercel.app/post/infix-prefix-postfix-notation
{--
  Parameter:
  infixr: the operator is right-associative
  infixl: the operator is left-associative
  infix: the operator is non-associative
--}

{--
<!-- 
  Assosiatif (Pengurutan)
  - Meskipun pengurutan berbeda, hasil yg dihasilkan sama
  10 + 5 + 5 = <a0> - <a1> + <a2> / Left associative (default)
  10 + (5 + 5) = <a0> - <a1> / Right associative
  
  Non associative: https://en.wikipedia.org/wiki/Associative_property#Non-associative_operation
 -->
--}

-- (/=) -> Prec = 8
-- (*) -> Prec = 7
-- (+) -> Prec = 6

-- # Right associative
(./=) :: Int -> Int -> Int
a ./= b = a + b
infixr 8 ./=

rightAssociative :: Int
rightAssociative = 1 * 2 ./= 2 -- 1 * (2 /= 2)

-- # Left associative
(.\=) :: Int -> Int -> Int
a .\= b = a * b
infixl 4 .\=

leftAssociative :: Int
leftAssociative = 2 + 6 .\= 5 

-- # Non associative
(.|=) :: Int -> Int -> Bool
a .|= b = a == b
infix 7 .|=

nonAssociative:: Bool
nonAssociative = 5 .|= 2