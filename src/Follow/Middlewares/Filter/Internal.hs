module Follow.Middlewares.Filter.Internal
  ( equalP
  ) where

import           Follow.Types (Entry, MFilterPredicate)

type EntryFieldGetter a = Entry -> Maybe a

equalP :: Eq a => EntryFieldGetter a -> a -> MFilterPredicate
equalP getter x entry = getter entry == Just x
