module Follow.Middlewares.Filter.Internal
  ( equalP
  , containP
  , prefixP
  , suffixP
  ) where

import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T (isInfixOf, isPrefixOf, isSuffixOf)
import           Follow.Types (Entry, MFilterPredicate)

type EntryFieldGetter a = Entry -> Maybe a

equalP :: Eq a => EntryFieldGetter a -> a -> MFilterPredicate
equalP = liftP (==)

containP :: EntryFieldGetter Text -> Text -> MFilterPredicate
containP = liftP T.isInfixOf

prefixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
prefixP = liftP T.isPrefixOf

suffixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
suffixP = liftP T.isSuffixOf

liftP :: (b -> a -> Bool) -> EntryFieldGetter a -> b -> MFilterPredicate
liftP p getter x entry = maybe False (p x) (getter entry)
