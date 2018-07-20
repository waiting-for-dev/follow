{- |
Description: Top namespace for follow application.

It contains the main data type for the application: a `Recipe`. A
recipe is the haskell representation of the information for something
to be followed.
-}
module Follow
  ( Recipe(..)
  ) where

import           Follow.Strategies (Arguments)

data Recipe = Recipe
  { rVersion     :: String -- ^ Version of the DSL used.
  , rTitle       :: String -- ^ Title for the recipe; what is being followed.
  , rDescription :: String -- ^ A description for the recipe
  , rTags        :: [String] -- ^ Tags that apply to the recipe
  , rArguments   :: Arguments -- ^ Arguments to be given to the strategy
  } deriving (Show)
