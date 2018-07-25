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
  , URLS
  , URL
  ) where

import           Data.Dynamic (Dynamic)
import           Text.Parsec  (ParseError, Parsec)

-- | A recipe is the haskell representation of the information needed
-- to follow some author or subject on the Internet.
data Recipe = Recipe
  { rVersion     :: String -- ^ Version of the DSL used.
  , rTitle       :: String -- ^ Title for the recipe; what is being followed.
  , rDescription :: String -- ^ A description for the recipe
  , rTags        :: [String] -- ^ Tags that apply to the recipe
  , rArguments   :: Arguments -- ^ Arguments to be given to the fetcher strategy
  } deriving (Show)

-- | Directory, a list of URLs about an author or subject being followed.
data Directory = Directory
  { cRecipe :: Recipe
  , cURLS   :: URLS
  }

-- List of `URL`.
type URLS = [URL]

-- | URL pointing to some `Directory` item.
data URL = URL
  { uURI         :: String
  , uGUID        :: String
  , uTitle       :: Maybe String
  , uDescription :: Maybe String
  , uAuthor      :: Maybe String
  }

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

-- | Function to fetch the URLs with content from the recipe
type Fetcher = Recipe -> URLS
