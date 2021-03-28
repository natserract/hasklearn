module Main where

import Lib
import Basic (fmt, fmtChar, lists, useLet)

main :: IO ()
main = do
  someFunc
  fmt "alfin" "surya"
  fmtChar ("alfin", "surya")
  lists ["List 0", "List 1", "List 2"]
  useLet 2
