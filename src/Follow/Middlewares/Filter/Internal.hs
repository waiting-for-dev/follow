{-|
Description: Predicate builders and combinators to be used in the
             filter middleware.

This module defines some predicate builders and combinators useful to
apply the filter middleware to a directory.

-}
module Follow.Middlewares.Filter.Internal
  ( Predicate
  , equalP
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
import           Follow.Types (Entry, EntryGetter)

-- | Predicate that takes an `Entry` as argument.
type Predicate = Entry -> Bool

-- | Builds a predicate which takes the field returned by the getter
-- and compares with given value for equality. It returns `False` when
-- field is `Nothing`.
equalP :: Eq a => EntryGetter a -> a -> Predicate
equalP = liftP (==)

-- | Builds a predicate which takes given value and checks whether it
-- is an infix for the field returned by given getter. It returns `False`
-- when field is `Nothing`.
infixP :: EntryGetter Text -> Text -> Predicate
infixP = liftP T.isInfixOf

-- | Builds a predicate which takes given value and checks whether it
-- is a prefix for the field returned by given getter. It returns `False`
-- when field is `Nothing`.
prefixP :: EntryGetter Text -> Text -> Predicate
prefixP = liftP T.isPrefixOf

-- | Builds a predicate which takes given value and checks whether it
-- is a suffix for the field returned by given getter. It returns `False`
-- when field is `Nothing`.
suffixP :: EntryGetter Text -> Text -> Predicate
suffixP = liftP T.isSuffixOf

-- | Builds a predicate which combines with a logical and given
-- predicates.
andP :: Predicate -> Predicate -> Predicate
andP = liftB (&&)

-- | Builds a predicate which combines with a logical or given
-- predicates.
orP :: Predicate -> Predicate -> Predicate
orP = liftB (||)

-- | Build a predicte which negates the result of given predicate.
notP :: Predicate -> Predicate
notP p entry = not $ p entry

liftP :: (b -> a -> Bool) -> EntryGetter a -> b -> Predicate
liftP p getter x entry = maybe False (p x) (getter entry)

liftB :: (Bool -> Bool -> Bool) -> Predicate -> Predicate -> Predicate
liftB f p q entry = p entry `f` q entry
