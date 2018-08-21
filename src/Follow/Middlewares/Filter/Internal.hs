module Follow.Middlewares.Filter.Internal
  ( equalP
  , infixP
  , prefixP
  , suffixP
  , andP
  , orP
  , notP
  ) where

import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T (isInfixOf, isPrefixOf, isSuffixOf)
import           Follow.Types (Entry, MFilterPredicate)

type EntryFieldGetter a = Entry -> Maybe a

equalP :: Eq a => EntryFieldGetter a -> a -> MFilterPredicate
equalP = liftP (==)

infixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
infixP = liftP T.isInfixOf

prefixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
prefixP = liftP T.isPrefixOf

suffixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
suffixP = liftP T.isSuffixOf

andP :: MFilterPredicate -> MFilterPredicate -> MFilterPredicate
andP = liftB (&&)

orP :: MFilterPredicate -> MFilterPredicate -> MFilterPredicate
orP = liftB (||)

notP :: MFilterPredicate -> MFilterPredicate
notP p entry = not $ p entry

liftP :: (b -> a -> Bool) -> EntryFieldGetter a -> b -> MFilterPredicate
liftP p getter x entry = maybe False (p x) (getter entry)

liftB ::
     (Bool -> Bool -> Bool)
  -> MFilterPredicate
  -> MFilterPredicate
  -> MFilterPredicate
liftB f p q entry = p entry `f` q entry
