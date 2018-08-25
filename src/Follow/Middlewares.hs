{-|
Description: Top namespace to define middleware strategies and global
             middleware functions.

Middlewares are strategies that modify a directory in some
way, like filtering or sorting. They are using between fetching and digesting subject.

This module acts like the top namespace to define middleware
strategies, along with functions to be used in the middleware scope.
-}
module Follow.Middlewares
  ( applyMiddlewares
  ) where

import           Follow.Types (Directory, Middleware)

-- | Applies, from left to right, given middlewares to the directory.
applyMiddlewares :: [Middleware] -> Directory -> Directory
applyMiddlewares middlewares directory =
  foldl (\directory middleware -> middleware directory) directory middlewares
