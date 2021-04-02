{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Basic.Variables where

{--
  Notes:
  -> All variable (default) in haskell can't be changed (immutable)
  -> Mutating a mutable variable is considered a side effect
  -> Different between variable & function: A function takes an
     argument/parameter and gives a result
  -> Haskell is pure functional lang, no side effect!
  -> See doc: https://wiki.haskell.org/Let_vs._Where
--}
variables = do
  print (x, y)
  print ("where: ", concatBorderPixelsW "10")
  print ("let: ", concatBorderPixelsL "10")
  print $ locales ("have", "indo")

-- # Basic variables
-- constant (immutable)
-- Basic variables (global variable)
x :: Integer
x = 6

y :: Integer
y = 5

-- Function
xyn :: Num a => (a, a) -> a
xyn (a, b) = a + b

{--
  -> # let (expression), where (clause)
  -> let var = expression in expression
  -> let area = pi * 5 ^ 2;

  You can define local variable using two ways,
  -> Using let
  -> Using where
--}

-- where clause
concatBorderPixelsW :: [Char] -> [Char]
concatBorderPixelsW bord_x = bord_y ++ bord_x where bord_y = "20"

-- let
concatBorderPixelsL :: [Char] -> [Char]
concatBorderPixelsL bord_x =
  let bord_y = bord_y ++ bord_x where bord_y = "20" in bord_y

-- nested let w condition
-- don't call let var in scope itself -> infinite loop
locales :: ([Char], [Char]) -> [Char]
locales (f, loc) = do
  let my_friends :: [[Char]]
      my_friends
      -- actually this is no need use let, 
      -- but for surely we can use nested let
        | loc == "france" =
          let france_friends = ["Rebecca", "John"] in france_friends
        | loc == "indo" =
          let indo_friends = ["Supriyadi", "Marduso"] in indo_friends
   in case f of
        "have" -> "Your friends is" ++ show my_friends
        "haven't" -> "Hired a friend!"
