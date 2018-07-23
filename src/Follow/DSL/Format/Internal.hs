{-
Description: Defines the base DSL from which a fetcher strategy can define extra items down below.
-}
module Follow.DSL.Format.Internal
  ( nameFormat
  , versionLineFormat
  , versionFormat
  , titleLineFormat
  , titleFormat
  , descriptionLineFormat
  , descriptionFormat
  , tagsLineFormat
  , tagsFormat
  , tagFormat
  , argumentsFormat
  , lineFormat
  , format
  ) where

import           Data.Char         (toUpper)
import           Data.Functor      (($>))
import           Follow.DSL.Format
import           Follow.Fetchers   (Arguments, ArgumentsDSL)
import           Text.Parsec

type Name = String

-- | Version of the DSL
type Version = String

-- | Title for the recipe
type Title = String

-- | Desctiption for the recipe
type Description = String

-- | A tag for the recipe
type Tag = String

-- | List of tags
type Tags = [Tag]

-- | Format expected for the DSL String
format :: ArgumentsDSL -> Parse (Version, Title, Description, Tags, Arguments)
format argumentsDSL = do
  version <- versionLineFormat
  endOfLine
  title <- titleLineFormat
  endOfLine
  description <- descriptionLineFormat
  endOfLine
  tags <- tagsLineFormat
  -- next endOfLine is parsed in argumentsFormat
  arguments <- argumentsFormat argumentsDSL
  optional endOfLine
  return (version, title, description, tags, arguments)

-- | Format for a line consisting of a name/value pair
lineFormat :: Name -> Parse a -> Parse a
lineFormat name valueFormat =
  optionalSpaceFormat *> nameFormat name *> spaceFormat *> valueFormat <*
  optionalSpaceFormat

-- | Format for the line containg the DSL version
versionLineFormat :: Parse Version
versionLineFormat = lineFormat "VERSION" versionFormat

-- | Format for valid version numbers
versionFormat :: Parse Version
versionFormat = choice $ string <$> ["1.0"]

-- | Format for the line containing the recipe title
titleLineFormat :: Parse Title
titleLineFormat = lineFormat "TITLE" titleFormat

-- | Format for the recipe title
titleFormat :: Parse Title
titleFormat = multiWordFormat

-- | Format for the line containing the recipe description
descriptionLineFormat :: Parse Description
descriptionLineFormat = lineFormat "DESCRIPTION" descriptionFormat

-- | Format for the recipe description
descriptionFormat :: Parse Description
descriptionFormat = multiWordFormat

-- | Format for the line containing the recipe tags
tagsLineFormat :: Parse Tags
tagsLineFormat = lineFormat "TAGS" tagsFormat

-- | Format for the recipe tags
tagsFormat :: Parse Tags
tagsFormat = csFormat tagFormat

-- | Format for a tag
tagFormat :: Parse Tag
tagFormat = unwords <$> sepBy1 (many1 (alphaNum <|> oneOf "_-")) spaceFormat

-- | Format for the arguments to expect for given dsl.
argumentsFormat :: ArgumentsDSL -> Parse Arguments
argumentsFormat dsl = sequence (toSteps dsl)
  where
    toSteps =
      map
        (\(name, format) -> ((,) name <$> (endOfLine *> lineFormat name format)))

-- | Format for the expected reserved words in the DSL. They must be
-- an uppercase string, so if a downcase string is given as argument it will be
-- converted beforehand.
nameFormat :: Name -> Parse ()
nameFormat name = string (toUpper <$> name) $> ()
