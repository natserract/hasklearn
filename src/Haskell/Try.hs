{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Haskell.Try where

tryTest = do
  print $ "1" -? "2" -? "3" -? "4" -? "5" -? "6"

-- Call function with prefix `func_name`
prefixFunc :: Num a => a -> a -> a
prefixFunc a b = a * b

z :: Num a => a -> a -> a
z a b = a `prefixFunc` b

-- data List t
--   = Nil
--   | t :. List t
--   deriving (Eq, Ord)

-- infixr 5 :.

-- infixr

(-?) :: a -> b -> [Char]
(-?) a b = show "Todo Error"

infixr 1 -?