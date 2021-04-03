module Haskell.Types where

types = do 
  print strType
  print integerType
  print charType
  print boolType
  print integersType
  fnType "Print Fn type"

-- # Basic Types
strType :: String
strType = "Str Type"

charType :: [Char]
charType = "Message" 

boolType :: Bool
boolType = False 

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

intType :: Int
intType = 1

integerType :: Integer
integerType = 1

integersType :: [Integer]
integersType = [2, 3, 4]

-- Haskell also has two types for floating point: Float and Double.
-- Float: f32 (rust), Double: f64 (rust)
floatType :: Float
floatType = 2.0

doubleType :: Double
doubleType = 2.0

-- # Type Function
-- Return Type: Monad IO 
fnType :: Show a => a -> IO ()
fnType = print