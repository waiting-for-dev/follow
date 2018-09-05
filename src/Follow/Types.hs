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
  , Recipe(..)
  , Subject(..)
  , Entry(..)
  , EntryGetter
  , Directory(..)
  , Result(..)
  , unwrapResult
  , Fetcher
  , Step
  , Fetched
  , FetchError(..)
  , FetchFeedError(..)
  , Middleware
  , Digester
  ) where

import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         runExceptT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Dynamic           (Dynamic)
import           Data.Text              (Text)
import           Data.Time              (UTCTime)
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
  , ePublishDate :: Maybe UTCTime -- ^ Item publish date.
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

-- | A final result of something. It has been obtained reaching the
-- outside world and contains either what is expected or a fetch
-- error. It is just a wrapper of `ExceptT`.
newtype Result a = Result
  { runResult :: ExceptT FetchError IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError FetchError)

-- | Unwraps a `Result` into its bare monad components. It is just a
-- helper to avoid having to call both `runResult` and `runExceptT`.
unwrapResult :: Result a -> IO (Either FetchError a)
unwrapResult = runExceptT . runResult

-- | Function to fetch a list of `Entry` from the outside world. See `Fetched`.
type Fetcher arguments = arguments -> Fetched

-- | Return value of a `Fetcher`: a list of `Entry` or an error (see `FetchError`).
type Fetched = Result [Entry]

-- | An error returned by a `Fetcher`.
data FetchError
  = URLWrongFormat
  | ResponseError R.HttpException
  | FetchFeedError FetchFeedError
  | TokenNotFound
  | TokenDecodingError
  deriving (Show)

-- | Errors returned by feed fetcher strategy. See `Follow.Fetchers.Feed`.
data FetchFeedError =
  FeedWrongFormat
  deriving (Show)

-- | Middlewares are strategies to modify a directory. They are used
-- after fetching entries but before digesting them.
type Middleware = Directory -> Directory

-- | A list of middlewares to be applied to some fetched entries.
type Step = (Fetched, [Middleware])

-- | A recipe is a specification of a complete strategy to create the
-- content to follow a subject.
data Recipe = Recipe
  { rSubject     :: Subject -- ^ The subject being followed.
  , rSteps       :: [Step] -- ^ List of steps to be applied.
  , rMiddlewares :: [Middleware] -- ^ List of middlewares to apply to the result of all steps.
  }

-- | Digesters are strategies to transform a directory into something
-- to be consumed by an end user.
type Digester a = Directory -> a
