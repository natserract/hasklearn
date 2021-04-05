-- Just list of errors faq what i solved
module Haskell.ErrorFaq where

{--
  # No instance for (Eq a) arising from a use of ‘==’ ...
  
  Code:
  f :: forall a . a -> Maybe Int
  
  Solution:
  f :: (forall a . a -> Maybe Int)


  # Couldn't match expected type ‘x -> Bool’ with actual type ‘Bool’
  -> Because parameter is not defined in function
  
  Code:
  largest:: forall x . x -> Bool
  largest = True -- Paramater x??
  
  Solution:
  largest:: forall x . x -> Bool
  largest x = True


  # Could not deduce (Eq y) arising from a use of ‘==’ ... add (Eq y) to the context of
  -> You must add type classes to the context

  Code:
  equalityTypeClasses :: (Num a) => (a, b) -> [Char]
  equalityTypeClasses (a, b)
      | a == 100 = "True"

  Solution:
  equalityTypeClasses :: (Eq a, Num a) => (a, b) -> [Char]
--}