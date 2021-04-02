module Mod ( 
    run,
) where
import Basic.Print ( printFmt )
import Basic.Operators ( operators )
import Basic.Comments ( comments )
import qualified Basic.Modules as Modules
import qualified Basic.Conditions as Conditions
import Basic.Types ( types )
import qualified Basic.Variables as Variables

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
    Variables.variables
