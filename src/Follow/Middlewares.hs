{-|
Description: Middlewares are strategies to transform directories.

Between fetching and digesting, it can be useful to transform a
directory in some way, like filtering, sorting or whatever.
-}
module Follow.Middlewares
  ( applyMiddlewares
  ) where

import           Follow.Types (Directory, Middleware)

-- | Applies, from left to right, given middlewares to the directory.
applyMiddlewares :: [Middleware] -> Directory -> Directory
applyMiddlewares middlewares directory =
  foldl (\directory middleware -> middleware directory) directory middlewares
