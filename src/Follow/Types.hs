{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , Subject(..)
  , Entry(..)
  , Directory(..)
  , Result(..)
  , Fetcher
  , Fetched
  , FetchError(..)
  , FetchFeedError(..)
  , Middleware
  , Digester
  ) where

import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Dynamic           (Dynamic)
import           Data.Text              (Text)
import qualified Network.HTTP.Req       as R (HttpException)
import           Text.Parsec            (ParseError, Parsec)

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
  } deriving (Eq, Show)

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

-- | A final result of something. It has been obtained reaching the
-- outside world and contains either what is expected or a fetch
-- error. It is just a wrapper of `ExceptT`.
newtype Result a = Result
  { runResult :: ExceptT FetchError IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError FetchError)

-- | Function to fetch a list of `Entry` from the outside world. See `Fetched`.
type Fetcher arguments = arguments -> Fetched

-- | Return value of a `Fetcher`: a list of `Entry` or an error (see `FetchError`).
type Fetched = Result [Entry]

-- | An error returned by a `Fetcher`.
newtype FetchError =
  FetchFeedError FetchFeedError
  deriving (Show)

-- | Errors returned by feed fetcher strategy. See `Follow.Fetchers.Feed`.
data FetchFeedError
  = URLWrongFormat
  | FeedWrongFormat
  | ResponseError R.HttpException
  deriving (Show)

-- | Middlewares are strategies to modify a directory. They are used
-- after fetching entries but before digesting them.
type Middleware = Directory -> Directory

-- | Digesters are strategies to transform a directory into something
-- to be consumed by an end user.
type Digester a = Directory -> a
