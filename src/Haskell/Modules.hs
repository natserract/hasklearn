
module Haskell.Modules where 

-- In Prelude module and Modules.hs, have same operator name (*), 
-- So for allowing this, we can hide Prelude operator, with hiding (..)
import Prelude hiding ((*))

modules = print "Modules"

{- 
  In Mod.hs using 'import qualified ...'
  The reason why use qualified because, in Modules.hs & Comments.hs has same name ('comments')
  So, in haskell for allowing this, just type qualified when import
--}
comments :: IO ()
comments = print "Comments from modules"

-- import Prelude hiding ((*))
(*) :: Int -> Int -> Int
x * 0 = 0
x * y = x * ( y - 1)