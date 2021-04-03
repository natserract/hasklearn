module Haskell.Comments where

comments = print "Comments from comments"
-- Inline Comment 
{- 
  Hello
  This is Long comment
-}

-- | A haskell doc -> Like jsdoc
type RequestCallback m a = a -> m ()
