module Haskell.Infix where

infixOp = do 
  print ""

-- Infix operation
-- Many people says, prefer to use infix syntax rather than prefix for binary operation
-- Because make expressions and statement of mathematical properties more readable
{--
  Common operation:
  infixr 8 ^                     -- exponentiation
  infixl 7 *                     -- multiplication
  infix  7 /                     -- division
  infixl 6 +, -                  -- addition, subtraction
  infixr 5 :                     -- cons
  infix  4 ==, /=, <, <=, >=, >  -- relational comparisons
  infixr 3 &&                    -- Boolean AND
  infixr 2 ||                    -- Boolean OR
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
