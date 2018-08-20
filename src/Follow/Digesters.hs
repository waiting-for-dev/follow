{- |
Description: Top namespace to define digester strategies to transform
             directories into something consumible by an user.
-}
module Follow.Digesters where

import           Follow.Types (Digester, Directory)

-- | Transform a directory into something using given digester
digest :: Digester a -> Directory -> a
digest digester = digester
