module Haskell.Init where

import Haskell.Comments (comments)
import qualified Haskell.Conditions as C
import qualified Haskell.Lists as L
import qualified Haskell.Modules as M
import Haskell.Operators (operators)
import Haskell.Print (printFmt)
import qualified Haskell.Variables as V
import Haskell.Infix
import Haskell.Functions
import Haskell.Types
import Haskell.GenericTypes
import Haskell.Try (tryCase, tryRoot)
import Haskell.Tree (rootTree)
import Haskell.Expression ( outExpr )

-- Monad IO
run :: IO ()
run = do
  printFmt
  operators
  comments
  M.modules
  M.comments
  C.conditions
  V.variables
  print L.lists 
  functions
  types
  infixOp
  genTypes
  tryRoot
  rootTree
  outExpr