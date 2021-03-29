
module Basic.Modules where 

modules = print "Modules"

{- 
  In Mod.hs using 'import qualified ...'
  The reason why use qualified because, in Modules.hs & Comments.hs has same name ('comments')
  So, in haskell for allowing this, just type qualified when import
--}
comments :: IO ()
comments = print "Comments from modules"