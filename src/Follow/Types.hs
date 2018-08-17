{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
Description: Global definition of types.
-}
module Follow.Types
  ( Parse
  , ParseResult
  , ArgumentName
  , Arguments
  , ArgumentsDSL
  , Fetcher
  , Recipe(..)
  , Directory(..)
  , Entries
  , Entry(..)
  , FetchError(..)
  , FetchFeedError(..)
  , Result(..)
  ) where

import           Control.Monad.Except   (ExceptT, MonadError, catchError,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Dynamic           (Dynamic)
import           Data.Text              (Text)
import qualified Network.HTTP.Req       as R (HttpException)
import           Text.Parsec            (ParseError, Parsec)

-- | A recipe is the haskell representation of the information needed
-- to follow some author or subject on the Internet.
data Recipe = Recipe
  { rVersion     :: String -- ^ Version of the DSL used.
  , rTitle       :: String -- ^ Title for the recipe; what is being followed.
  , rDescription :: String -- ^ A description for the recipe
  , rTags        :: [String] -- ^ Tags that apply to the recipe
  , rArguments   :: Arguments -- ^ Arguments to be given to the fetcher strategy
  } deriving (Show)

-- | Directory, a list of Entries about an author or subject being followed.
data Directory = Directory
  { dRecipe  :: Recipe
  , dEntries :: Entries
  } deriving (Show)

-- List of `Entry`.
type Entries = [Entry]

-- | Entry for some URI pointing to some `Directory` item.
data Entry = Entry
  { eURI         :: Text
  , eGUID        :: Text
  , eTitle       :: Maybe Text
  , eDescription :: Maybe Text
  , eAuthor      :: Maybe Text
  } deriving (Eq, Show)

-- Type alias for the common parsing format for the DSL: from a string
-- to whatever without any state.
type Parse = Parsec String ()

-- | The result of a parsing: a `Follow.Recipe` or an error.
type ParseResult = Either ParseError Recipe

type ArgumentName = String

-- | List of arguments; pairs of name and value.
type Arguments = [(ArgumentName, Dynamic)]

-- | Concrete fetchers must define an ArgumentsDSL method that
-- defines a mapping between argument names and its expected DSL for the
-- value.
type ArgumentsDSL = [(ArgumentName, Parsec String () Dynamic)]

-- | A final result, which has been obtained reaching the outside
-- world and contains either what is expected or a fetch error.
newtype Result a = Result
  { runResult :: ExceptT FetchError IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError FetchError)

-- | Function to fetch the Entries with content from the recipe
type Fetcher = Recipe -> Result Entries

-- | Any kind of error returned by any fetcher strategy. See `Follow.Fetchers`.
newtype FetchError =
  FetchFeedError FetchFeedError
  deriving (Show)

-- | Errors returned by feed fetcher strategy. See `Follow.Fetchers.Feed`.
data FetchFeedError
  = URLFromDynamicConversionFailure
  | URLWrongFormat
  | FeedWrongFormat
  | ResponseError R.HttpException
  deriving (Show)
