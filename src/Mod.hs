module Mod ( 
    run,
) where

import Basic.Print
import Basic.Operators ( operators )
import Basic.Comments
import qualified Basic.Modules as Modules
import qualified Basic.Conditions as Conditions
import Basic.Types

-- Monad IO
run :: IO ()
run = do
    printFmt
    operators
    comments
    Modules.modules
    Modules.comments
    types
    Conditions.conditions
