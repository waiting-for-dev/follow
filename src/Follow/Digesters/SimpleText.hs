{-|
 Description: Digests a subject into a textual representation.
-}
{-# LANGUAGE OverloadedStrings #-}

module Follow.Digesters.SimpleText
  ( digester
  ) where

import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T (concat, intercalate, replicate)
import           Follow.Types (Digester, Directory (..), Entry (..),
                               Header (..))

-- | See `Follow.Types.Digester+ .
digester :: Digester Text
digester directory =
  let header = dHeader directory
      headerTitle = hTitle header
      headerDescription = hDescription header
      headerTags = hTags header
      entries = dEntries directory
   in T.intercalate
        emptyLine
        [ headerTitle
        , headerDescription
        , T.intercalate "," headerTags
        , headerSeparator
        , digestEntries entries
        ]
  where
    headerSeparator :: Text
    headerSeparator = T.replicate 80 "#"

digestEntries :: [Entry] -> Text
digestEntries entries = T.intercalate entrySeparator $ map digestEntry entries
  where
    entrySeparator :: Text
    entrySeparator = T.concat ["\n", T.replicate 80 "-", "\n"]
    digestEntryItem :: Maybe Text -> Text
    digestEntryItem = fromMaybe ""
    digestEntry :: Entry -> Text
    digestEntry entry =
      let entryURI = eURI entry
          entryTitle = eTitle entry
          entryDescription = eDescription entry
          entryAuthor = eAuthor entry
       in T.intercalate
            emptyLine
            (digestEntryItem <$>
             [entryURI, entryTitle, entryDescription, entryAuthor])

emptyLine :: Text
emptyLine = "\n\n\n"
