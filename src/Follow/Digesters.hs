{- |
Description: Top namespace to define digester strategies and global
             digesting functions.
-}
module Follow.Digesters
  ( digest
  ) where

import           Follow.Types (Digester, Directory)

-- | Digests a directory, using given digester, into something
-- consumible by an end user.
digest :: Digester a -> Directory -> a
digest digester = digester
