{-
Description: Functions for further transformation of results.

When you fetch and digest a recipe, you get something that can be
consumed by an end user. However, sometimes you need to further
process it, like writting to a file or sending to a database.

-}
module Follow.Results
  ( WritableToFile(..)
  ) where

import           Control.Monad.Except (runExceptT)
import           Data.Text            (Text)
import           Data.Text.IO         as T (writeFile)
import           Follow.Types         (Result (..))

-- | Something that can be written to a file.
class WritableToFile a where
  writeToFile :: FilePath -> a -> IO () -- ^ Writes content to given file path.

instance WritableToFile Text where
  writeToFile = T.writeFile

-- | A result can be written to a file if two conditions met: 1) Its
-- inner type is `WritableToFile`. 2) It is a successful result. If it
-- contains an error, it won't write anything and it will be silent about
-- it.
instance WritableToFile a => WritableToFile (Result a) where
  writeToFile path result = do
    result' <- runExceptT $ runResult result
    case result' of
      Left error -> return ()
      Right x    -> writeToFile path x
