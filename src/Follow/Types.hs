{-|
Description: Definition of types.

This module defines the types used in the whole application.
-}
module Follow.Types
  ( Recipe(..)
  , Subject(..)
  , Entry(..)
  , Fetched
  , EntryGetter
  , Directory(..)
  , Step
  , Middleware
  , Digester
  ) where

import           Data.Text (Text)
import           Data.Time (LocalTime)
import           Data.Yaml (Object, Parser)

-- | Subject being followed. The whole idea of `Follow` is being able
-- to build strategies to gather URIs for the content published about any
-- subject.
data Subject = Subject
  { sTitle       :: Text -- ^ Title.
  , sDescription :: Text -- ^ Description.
  , sTags        :: [Text] -- ^ List of tags.
  } deriving (Eq, Show)

-- | An item of content that has been published somewhere.
data Entry = Entry
  { eURI         :: Maybe Text -- ^ URI that identifies the location of the item.
  , eGUID        :: Maybe Text -- ^ Unique identifier.
  , eTitle       :: Maybe Text -- ^ Title.
  , eDescription :: Maybe Text -- ^ Description.
  , eAuthor      :: Maybe Text -- ^ Author.
  , ePublishDate :: Maybe LocalTime -- ^ Item publish date.
  } deriving (Eq, Show)

-- | Entries fetched usually from the outside world.
type Fetched m = m [Entry]

-- | Function that returns a field from an `Entry`. They are the -
-- automatically generated methods for the `Entry` record.
type EntryGetter a = Entry -> Maybe a

-- | Gathering of `Item` published for some `Subject`.
data Directory = Directory
  { dSubject :: Subject -- ^ The subject.
  , dEntries :: [Entry] -- ^ The list of entries.
  } deriving (Eq, Show)

-- | Middleware does something to a directory.
type Middleware = Directory -> Directory

-- | A list of middlewares to be applied to some fetched entries.
type Step m = (Fetched m, [Middleware])

-- | A recipe is a specification of a complete strategy to create the
-- content to follow a subject.
data Recipe m = Recipe
  { rSubject     :: Subject -- ^ The subject being followed.
  , rSteps       :: [Step m] -- ^ List of steps to be applied.
  , rMiddlewares :: [Middleware] -- ^ List of middlewares to apply to the result of all steps.
  }

-- | Digesters are strategies to transform a directory into something
-- to be consumed by an end user.
type Digester a = Directory -> a
