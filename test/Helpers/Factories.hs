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

import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Follow.Fetchers.WebScraping (Selector (..))
import           Follow.Types                (Directory (..), Entry (..),
                                              Recipe (..), Subject (..))

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

_recipe :: (MonadCatch m, MonadThrow m) => Recipe m
_recipe = Recipe {rSubject = _subject, rSteps = [], rMiddlewares = []}

e2F :: (MonadCatch m, MonadThrow m) => Entry -> m [Entry]
e2F entry = return [entry]

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
