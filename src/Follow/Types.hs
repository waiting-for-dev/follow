{-|
Description: Definition of types.

This module defines the types used in the whole application.
-}
module Follow.Types
  ( Parse
  , ParseResult
  , ArgumentName
  , Arguments
  , ArgumentsDSL
  , Recipe(..)
  , Subject(..)
  , Entry(..)
  , EntryGetter
  , Directory(..)
  , Step
  , Middleware
  , Digester
  ) where

import           Data.Dynamic (Dynamic)
import           Data.Text    (Text)
import           Data.Time    (LocalTime)
import           Text.Parsec  (ParseError, Parsec)

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

-- | Function that returns a field from an `Entry`. They are the -
-- automatically generated methods for the `Entry` record.
type EntryGetter a = Entry -> Maybe a

-- | Gathering of `Item` published for some `Subject`.
data Directory = Directory
  { dSubject :: Subject -- ^ The subject.
  , dEntries :: [Entry] -- ^ The list of entries.
  } deriving (Eq, Show)

-- Type alias for the common parsing format for the DSL: from a string
-- to whatever without any state.
type Parse = Parsec String ()

-- | The result of a parsing: a `Follow.Subject` or an error.
type ParseResult = Either ParseError Subject

type ArgumentName = String

-- | List of arguments; pairs of name and value.
type Arguments = [(ArgumentName, Dynamic)]

-- | Concrete fetchers must define an ArgumentsDSL method that
-- defines a mapping between argument names and its expected DSL for the
-- value.
type ArgumentsDSL = [(ArgumentName, Parse Dynamic)]

type Middleware = Directory -> Directory

-- | A list of middlewares to be applied to some fetched entries.
type Step m = (m [Entry], [Middleware])

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
