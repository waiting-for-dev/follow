{-# LANGUAGE OverloadedStrings #-}

-- | Out of the box built record to be used in testing.
module Helpers.Factories
  ( _subject
  , _entry
  , _directory
  , _recipe
  , _selector
  , e2F
  ) where

import           Follow.Fetchers.WebScraping (Selector (..))
import           Follow.Types                (Directory (..), Entry (..),
                                              Fetched, Recipe (..),
                                              Subject (..))

_entry :: Entry
_entry =
  Entry
    { eURI = Nothing
    , eGUID = Nothing
    , eTitle = Nothing
    , eDescription = Nothing
    , eAuthor = Nothing
    , ePublishDate = Nothing
    }

_subject :: Subject
_subject =
  Subject {sTitle = "Title", sDescription = "Description", sTags = ["tag"]}

_directory :: Directory
_directory = Directory {dSubject = _subject, dEntries = [_entry]}

_recipe :: Recipe
_recipe = Recipe {rSubject = _subject, rSteps = [], rMiddlewares = []}

e2F entry = return [entry] :: Fetched

_selector :: Selector
_selector =
  Selector
    { selURI = Nothing
    , selGUID = Nothing
    , selTitle = Nothing
    , selDescription = Nothing
    , selAuthor = Nothing
    , selPublishDate = Nothing
    }
