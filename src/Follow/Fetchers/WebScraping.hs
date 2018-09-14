{-|
Description: Fetcher strategy to scrap info from an HTML URI.

This module is the namespace to define a fetcher strategy which
generates entries scraping the contents requested to an HTML URI.

A `Selector` must be given in order to know from where the information
for each entry field should be taken.

Be aware that scraping an HTML page has very few consistency
warantee. So, depending on the page structure and the selector you
give, you could end up with 5 URIs, 4 titles and 6 descriptions. Keep
in mind that the leading and limiting asset are the URIs, so in the
previous scenario one `Nothing` title would be added and one
description would be discarded.

Here it is an example:

@
import Follow
import Follow.Fetchers.WebScraping

selector :: Selector
selector = Selector {
    selURI = Just $ Attr ".title a" "href"
  , selGUID = Just $ Attr ".title a" "href"
  , selTitle = Just $ InnerText ".title a"
  , selDescription = Just $ InnerText ".description"
  , selAuthor = Just $ InnerText ".author"
  , selPublishDate = Nothing
}

result :: IO [Entry]
result = fetch ("http://an_url.com", selector)
@
-}
module Follow.Fetchers.WebScraping
  ( fetch
  , Selector(..)
  , SelectorItem(..)
  , CSSSelector
  , HTMLAttribute
  ) where

import           Control.Monad.Catch                  (MonadThrow)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import qualified Data.ByteString                      as BS (ByteString)
import qualified Data.ByteString.Lazy                 as BL (ByteString)
import           Data.Text                            (Text)
import           Follow.Fetchers.WebScraping.Internal (CSSSelector,
                                                       HTMLAttribute,
                                                       Selector (..),
                                                       SelectorItem (..),
                                                       htmlToEntries)
import           Follow.Types                         (Fetched)
import           HTTP.Follow                          (getResponseBody,
                                                       parseUrl)
import           Network.HTTP.Req                     (MonadHttp)

-- | Fetches entries from given url using specified selectors.
fetch ::
     (MonadThrow m, MonadIO m, MonadHttp m)
  => BS.ByteString
  -> Selector
  -> Fetched m
fetch url selector = do
  url' <- parseUrl url
  response <- getResponseBody url'
  liftIO $ htmlToEntries response selector
