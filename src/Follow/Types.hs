{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Description: Global definition of types.
-}
module Follow.Types
  ( Parse
  , ParseResult
  , ArgumentName
  , Arguments
  , ArgumentsDSL
  , Fetcher
  , Subject(..)
  , Directory(..)
  , Entry(..)
  , FetchError(..)
  , FetchFeedError(..)
  , Result(..)
  , Digester
  , Middleware
  , Fetched
  ) where

import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Dynamic           (Dynamic)
import           Data.Text              (Text)
import qualified Network.HTTP.Req       as R (HttpException)
import           Text.Parsec            (ParseError, Parsec)

-- | Subject to follow
data Subject = Subject
  { sTitle       :: Text -- ^ Title of what is being followed.
  , sDescription :: Text -- ^ A description
  , sTags        :: [Text] -- ^ Tags that apply
  } deriving (Eq, Show)

-- | Directory, a list of entries about an author or subject being followed.
data Directory = Directory
  { dSubject :: Subject
  , dEntries :: [Entry]
  } deriving (Show)

-- | Entry for some URI pointing to some `Directory` item.
data Entry = Entry
  { eURI         :: Maybe Text
  , eGUID        :: Maybe Text
  , eTitle       :: Maybe Text
  , eDescription :: Maybe Text
  , eAuthor      :: Maybe Text
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

-- | A final result, which has been obtained reaching the outside
-- world and contains either what is expected or a fetch error.
newtype Result a = Result
  { runResult :: ExceptT FetchError IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError FetchError)

-- | Function to fetch entries
type Fetcher a = a -> Fetched

type Fetched = Result [Entry]

-- | Any kind of error returned by any fetcher strategy. See `Follow.Fetchers`.
newtype FetchError =
  FetchFeedError FetchFeedError
  deriving (Show)

-- | Errors returned by feed fetcher strategy. See `Follow.Fetchers.Feed`.
data FetchFeedError
  = URLWrongFormat
  | FeedWrongFormat
  | ResponseError R.HttpException
  deriving (Show)

-- | Digesters are strategies to transform a directory into something
-- to be consumed by a user.
type Digester a = Directory -> a

-- | Middlewares are strategies transforming directories.
type Middleware = Directory -> Directory
