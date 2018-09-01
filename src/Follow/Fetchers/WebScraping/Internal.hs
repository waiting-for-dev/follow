{-|
Description: Internals for the web scraping fetching strategy.

This module contains the inner wiring for the scraping fetching
strategy.
-}
module Follow.Fetchers.WebScraping.Internal
  ( Selector(..)
  , SelectorItem(..)
  , CSSSelector
  , HTMLAttribute
  , htmlToEntries
  ) where

import           Control.Monad              (join)
import           Control.Monad.Except       (liftIO)
import qualified Data.ByteString            as BS (ByteString)
import qualified Data.ByteString.Lazy       as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (unpack)
import           Data.List                  (uncons)
import           Data.Text                  (Text)
import qualified Data.Text                  as T (pack, unpack)
import           Data.Time                  (UTCTime)
import           Data.Time.Follow           (parseTimeGuess)
import           Data.Tree.NTree.TypeDefs   (NTree)
import           Follow.Types               (Entry (..))
import           Text.HandsomeSoup          (css, parseHtml, (!))
import           Text.XML.HXT.Core          (IOSArrow, XNode, getChildren,
                                             getText, isText, runX, (>>>))
import           Text.XML.HXT.DOM.TypeDefs  (XmlTree)

-- | Data type with the selectors to use when scraping each `Entry`
-- item.
data Selector = Selector
  { selURI         :: Maybe SelectorItem
  , selGUID        :: Maybe SelectorItem
  , selTitle       :: Maybe SelectorItem
  , selDescription :: Maybe SelectorItem
  , selAuthor      :: Maybe SelectorItem
  , selPublishDate :: Maybe SelectorItem
  }

-- | Selector to use when scraping an `Entry` item.
data SelectorItem
  = InnerText CSSSelector -- ^ This selector will take the inner text
  -- immediately descendant of a tag selected with given css selector.
  | Attr CSSSelector
         HTMLAttribute -- ^ This selector will take the value of given
  -- argument in the tag matched by given css selector.

-- | A CSS2 selector.
type CSSSelector = Text

-- | An HTML attribute name.
type HTMLAttribute = Text

type Doc b = IOSArrow b (NTree XNode)

type SLinks = [Text]

type SGuids = [Text]

type STitles = [Text]

type SDescriptions = [Text]

type SAuthors = [Text]

type SPublishDates = [Text]

-- | Converts a bytestring with HTML content to a list of entries,
-- scraping entry items using given selector. The return type is
-- wrapped within an IO because of the underlying vendor API.
htmlToEntries :: BL.ByteString -> Selector -> IO [Entry]
htmlToEntries html selector = do
  links <- scrap doc (selURI selector)
  guids <- scrap doc (selGUID selector)
  titles <- scrap doc (selTitle selector)
  descriptions <- scrap doc (selDescription selector)
  authors <- scrap doc (selAuthor selector)
  publishDates <- scrap doc (selPublishDate selector)
  return $ entriesFromItems links guids titles descriptions authors publishDates
  where
    doc = parseHtml $ BL.unpack html

entriesFromItems ::
     SLinks
  -> SGuids
  -> STitles
  -> SDescriptions
  -> SAuthors
  -> SPublishDates
  -> [Entry]
entriesFromItems [] _guids _titles _descriptions _authors _publishDates = []
entriesFromItems (u:us) guids titles descriptions authors publishDates =
  let (g, gs) = extract guids id
      (t, ts) = extract titles id
      (d, ds) = extract descriptions id
      (a, as) = extract authors id
      (p, ps) = extract publishDates parseTimeGuess
   in Entry
        { eURI = Just u
        , eGUID = g
        , eTitle = t
        , eDescription = d
        , eAuthor = a
        , ePublishDate = join p
        } :
      entriesFromItems us gs ts ds as ps
  where
    extract :: [a] -> (a -> b) -> (Maybe b, [a])
    extract l f =
      case uncons l of
        Nothing     -> (Nothing, [])
        Just (h, t) -> (Just (f h), t)

scrap :: Doc XmlTree -> Maybe SelectorItem -> IO [Text]
scrap _doc Nothing = return []
scrap doc (Just (InnerText selector)) =
  let items =
        doc >>> css (T.unpack selector) >>> getChildren >>> isText >>> getText
   in fmap T.pack <$> runX items
scrap doc (Just (Attr selector attr)) =
  let items = doc >>> css (T.unpack selector) ! T.unpack attr
   in fmap T.pack <$> runX items
