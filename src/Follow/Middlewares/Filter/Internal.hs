{-|
Description: Predicate builders and combinators.
-}
module Follow.Middlewares.Filter.Internal
  ( equalP
  , infixP
  , prefixP
  , suffixP
  , andP
  , orP
  , notP
  , MFilterPredicate
  ) where

import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T (isInfixOf, isPrefixOf, isSuffixOf)
import           Follow.Types (Entry)

type MFilterPredicate = Entry -> Bool

type EntryFieldGetter a = Entry -> Maybe a

-- | Predicate that returns `True` on equality.
equalP :: Eq a => EntryFieldGetter a -> a -> MFilterPredicate
equalP = liftP (==)

-- | Predicate that returns `True` when given value is infix for the field.
infixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
infixP = liftP T.isInfixOf

-- | Predicate that returns `True` when given value is prefix for the field.
prefixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
prefixP = liftP T.isPrefixOf

-- | Predicate that returns `True` when given value is suffix for the field.
suffixP :: EntryFieldGetter Text -> Text -> MFilterPredicate
suffixP = liftP T.isSuffixOf

-- | Combines two predicates with a boolean and.
andP :: MFilterPredicate -> MFilterPredicate -> MFilterPredicate
andP = liftB (&&)

-- | Combines two predicates with a boolean or.
orP :: MFilterPredicate -> MFilterPredicate -> MFilterPredicate
orP = liftB (||)

-- | Inverts a predicate.
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
