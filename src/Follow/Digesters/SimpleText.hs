{-# LANGUAGE OverloadedStrings #-}

{-|
Description: Digests a subject into a textual representation.

This module defines a digester which justs takes the directory and
transforms it into a text with a minimal format.
-}
module Follow.Digesters.SimpleText
  ( digest
  ) where

import           Data.Maybe   (fromMaybe)
import           Data.Text    (Text)
import qualified Data.Text    as T (concat, intercalate, replicate)
import           Follow.Types (Digester, Directory (..), Entry (..),
                               Subject (..))

-- | The digester strategy to transform a directory into a simple text
-- representation.
digest :: Digester Text
digest directory =
  let header = dSubject directory
      headerTitle = sTitle header
      headerDescription = sDescription header
      headerTags = sTags header
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
