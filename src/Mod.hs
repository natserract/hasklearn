module Mod
  ( run,
  )
where

import Basic.Comments (comments)
import qualified Basic.Conditions as Conditions
import qualified Basic.Lists as Lists
import qualified Basic.Modules as Modules
import Basic.Operators (operators)
import Basic.Print (printFmt)
import qualified Basic.Variables as Variables

-- Monad IO
run :: IO ()
run = do
  printFmt
  operators
  comments
  Modules.modules
  Modules.comments
  Conditions.conditions
  Variables.variables
  Lists.lists ["List 1", "List 2"]
