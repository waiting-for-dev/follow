{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
Description: Instances to parse Follow types from YAML or JSON.

This module contains `Data.Yaml.FromJSON` instances in order to be
able to parse Follow types up to a `Recipe` from a yaml or json text.

You can use decode functions in "Data.Yaml" in order to do the parsing.

For time fields, any format accepted by
`Data.Time.Follow.parseTimeGuess` is accepted.
-}
module Follow.Parser
  (
  ) where

import           Control.Monad.Catch         (MonadThrow, throwM)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.ByteString             (ByteString)
import           Data.Text                   (Text)
import qualified Data.Text                   as T (unpack)
import qualified Data.Text.Encoding          as T (encodeUtf8)
import           Data.Time                   (LocalTime)
import           Data.Time.Follow            (parseTimeGuess)
import           Data.Yaml
import qualified Follow.Fetchers.Feed        as F.Feed (fetch)
import qualified Follow.Fetchers.WebScraping as F.WebScraping (Selector (..),
                                                               SelectorItem (..),
                                                               fetch)
import qualified Follow.Middlewares.Decode   as M.Decode (Encoding (..), apply)
import qualified Follow.Middlewares.Filter   as M.Filter (Predicate (..), andP,
                                                          apply, equalP,
                                                          greaterP, infixP,
                                                          lessP, notP, orP,
                                                          prefixP, suffixP)
import qualified Follow.Middlewares.Sort     as M.Sort (ComparisonFunction,
                                                        apply, byGetter)
import           Follow.Types                (Entry (..), EntryGetter, Fetched,
                                              Middleware, Recipe (..), Step,
                                              Subject (..))
import           Network.HTTP.Req            (MonadHttp)

type FetcherParser m = Object -> Parser (Fetched m)

type MiddlewareParser = Object -> Parser Middleware

data ParsedEntryGetter
  = PEG (EntryGetter Text)
  | PET (EntryGetter LocalTime)

-- | @
--   title: Title
--   description: Description
--   tags: [tag_1, tag_2]
--   @
instance FromJSON Subject where
  parseJSON =
    withObject "Subject" $ \v -> do
      title <- v .: "title"
      description <- v .: "description"
      tags <- v .: "tags"
      return $
        Subject {sTitle = title, sDescription = description, sTags = tags}

-- | @
--   type: text
--   options:
--     css: .selector
--   @
--
--   or
--
--   @
--   type: attr
--   options:
--     css: .link
--     name: href
--   @
instance FromJSON F.WebScraping.SelectorItem where
  parseJSON =
    withObject "SelectorItem" $ \v -> do
      kind <- v .: "type"
      options <- v .: "options"
      case (kind :: Text) of
        "attr" -> do
          css <- options .: "css"
          attr <- options .: "name"
          return $ F.WebScraping.Attr css attr
        "text" -> do
          css <- options .: "css"
          return $ F.WebScraping.InnerText css
        x -> fail $ concat ["Unknown type '", T.unpack x, "' for selector item"]

-- | @
--   uri: # See `SelectorItem` instance
--   title: null
--   description: null
--   guid: null
--   author: null
--   publish_date: null
--  @
instance FromJSON F.WebScraping.Selector where
  parseJSON =
    withObject "Selector" $ \v -> do
      uri <- v .: "uri"
      guid <- v .: "guid"
      title <- v .: "title"
      description <- v .: "description"
      author <- v .: "author"
      publishDate <- v .: "publish_date"
      return $
        F.WebScraping.Selector
          { F.WebScraping.selURI = uri
          , F.WebScraping.selGUID = guid
          , F.WebScraping.selTitle = title
          , F.WebScraping.selDescription = description
          , F.WebScraping.selAuthor = author
          , F.WebScraping.selPublishDate = publishDate
          }

-- | @utf8@, @utf16le@, @utf16be@, @utf32le@ or @utf32be@
instance FromJSON M.Decode.Encoding where
  parseJSON (String "utf8") = return M.Decode.UTF8
  parseJSON (String "utf16le") = return M.Decode.UTF16LE
  parseJSON (String "utf16be") = return M.Decode.UTF16BE
  parseJSON (String "utf32le") = return M.Decode.UTF32LE
  parseJSON (String "utf32be") = return M.Decode.UTF32BE
  parseJSON (String x) =
    fail $ concat ["Unknown type '", T.unpack x, "' for encoding item"]

instance FromJSON ParsedEntryGetter where
  parseJSON (String "title") = return $ PEG eTitle
  parseJSON (String "description") = return $ PEG eDescription
  parseJSON (String "uri") = return $ PEG eURI
  parseJSON (String "guid") = return $ PEG eGUID
  parseJSON (String "author") = return $ PEG eAuthor
  parseJSON (String "publish_date") = return $ PET ePublishDate
  parseJSON (String x) =
    fail $
    concat ["Unknown field '", T.unpack x, "' for by field comparison function"]

-- | @
--   type: by_field
--   options:
--     field: title
--   @
instance FromJSON M.Sort.ComparisonFunction where
  parseJSON =
    withObject "ComparisonFunction" $ \v -> do
      kind <- v .: "type"
      options <- v .: "options"
      case (kind :: Text) of
        "by_field" -> do
          field <- options .: "field"
          case field of
            PEG g -> return $ M.Sort.byGetter g
            PET g -> return $ M.Sort.byGetter g
        x ->
          fail $
          concat ["Unknown function '", T.unpack x, "' for comparison function"]

-- | @
--   type: equal
--   options:
--     field: title
--     value: Title
--   @
--
--   or
--
--   @
--   type: less
--   options:
--     field: publish_date
--     value: 2018-08-08 00:00:00
--   @
--
--   or
--
--   @
--   type: greater
--   options:
--     field: publish_date
--     value: 2018-08-08 00:00:00
--   @
--
--   or
--
--   @
--   type: infix
--   options:
--     field: title
--     value: something
--   @
--
--   or
--
--   @
--   type: prefix
--   options:
--     field: title
--     value: The
--   @
--
--   or
--
--   @
--   type: suffix
--   options:
--     field: title
--     value: end
--   @
--
--   or
--
--   @
--   type: not
--   options:
--     operator: # See `Predicate` instance
--   @
--
--   or
--
--   @
--   type: and
--   options:
--     operator1: # See `Predicate` instance
--     operator2: # See `Predicate` instance
--   @
--
--   or
--
--   @
--   type: or
--   options:
--     operator1: # See `Predicate` instance
--     operator2: # See `Predicate` instance
--   @
instance FromJSON M.Filter.Predicate where
  parseJSON =
    withObject "Predicate" $ \v -> do
      kind <- v .: "type"
      options <- v .: "options"
      case (kind :: Text) of
        "equal" -> do
          getter <- options .: "field"
          value <- options .: "value"
          case getter of
            PEG g -> returnTextFilter g value M.Filter.equalP
            PET g -> returnTimeFilter g value M.Filter.equalP
        "less" -> do
          getter <- options .: "field"
          value <- options .: "value"
          case getter of
            PEG g -> returnTextFilter g value M.Filter.lessP
            PET g -> returnTimeFilter g value M.Filter.lessP
        "greater" -> do
          getter <- options .: "field"
          value <- options .: "value"
          case getter of
            PEG g -> returnTextFilter g value M.Filter.greaterP
            PET g -> returnTimeFilter g value M.Filter.greaterP
        "infix" -> dispatchTextFilter options M.Filter.infixP "infix"
        "prefix" -> dispatchTextFilter options M.Filter.prefixP "prefix"
        "suffix" -> dispatchTextFilter options M.Filter.suffixP "suffix"
        "not" -> do
          operation <- options .: "operation"
          return $ M.Filter.notP operation
        "and" -> do
          operation1 <- options .: "operation1"
          operation2 <- options .: "operation2"
          return $ M.Filter.andP operation1 operation2
        "or" -> do
          operation1 <- options .: "operation1"
          operation2 <- options .: "operation2"
          return $ M.Filter.orP operation1 operation2
    where
      returnTextFilter getter value builder = return $ builder getter value
      returnTimeFilter getter value builder =
        let time = parseTimeGuess value :: Maybe LocalTime
         in case time of
              Nothing -> fail "Format for time is unknown"
              Just t  -> return $ builder getter t
      dispatchTextFilter object builder name = do
        getter <- object .: "field"
        value <- object .: "value"
        case getter of
          PEG g -> return $ builder value g
          PET g ->
            fail $
            concat
              [ "Tried to apply '"
              , name
              , "' filter with a field which is not text"
              ]

-- | @
--   type: feed
--   options:
--     url: http://someurl.com
--   @
--
--   or
--
--   @
--   type: webscraping
--   options:
--     url: http://someurl.com
--     selector: # See `Selector` instance
--   @
instance (MonadThrow m, MonadHttp m) => FromJSON (Fetched m) where
  parseJSON =
    withObject "Fetcher" $ \v -> do
      kind <- v .: "type"
      options <- v .: "options"
      dispatchToFetcher kind options

-- | @
--   type: decode
--   options:
--     encoding: # See `Encoding` instance
--  @
--
--  or
--
--  @
--   type: sort
--   options:
--     function: # See `ComparisonInstance` instance
--  @
--
--  or
--
--  @
--   type: filter
--   options:
--     operation: # See `Predicate` instance
--  @
instance FromJSON Middleware where
  parseJSON =
    withObject "Middleware" $ \v -> do
      kind <- v .: "type"
      options <- v .: "options"
      dispatchToMiddleware kind options

-- | @
--   subject: # See `Subject` instance
--   steps:
--     -
--       - # See `Fetched` instance
--       -
--         - # See `Middleware` instance
--   middlewares:"
--     - # See `Middleware` instance
--  @
instance (MonadThrow m, MonadHttp m) => FromJSON (Recipe m) where
  parseJSON =
    withObject "Recipe" $ \v -> do
      subject <- v .: "subject"
      steps <- v .: "steps"
      middlewares <- v .: "middlewares"
      return
        Recipe {rSubject = subject, rSteps = steps, rMiddlewares = middlewares}

dispatchToFetcher ::
     (MonadThrow m, MonadHttp m) => Text -> Value -> Parser (Fetched m)
dispatchToFetcher kind options =
  case kind of
    "feed"        -> withObject "Options" parseFFeed options
    "webscraping" -> withObject "Options" parseFWebScraping options

parseFFeed :: (MonadThrow m, MonadHttp m) => FetcherParser m
parseFFeed o = do
  url <- o .: "url"
  return $ F.Feed.fetch (T.encodeUtf8 url)

parseFWebScraping :: (MonadThrow m, MonadHttp m) => FetcherParser m
parseFWebScraping o = do
  url <- o .: "url"
  selector <- o .: "selector"
  return $ F.WebScraping.fetch (T.encodeUtf8 url) selector

dispatchToMiddleware :: Text -> Value -> Parser Middleware
dispatchToMiddleware kind options =
  case kind of
    "decode" -> withObject "Options" parseMDecode options
    "sort"   -> withObject "Options" parseMSort options
    "filter" -> withObject "Options" parseMFilter options

parseMDecode :: MiddlewareParser
parseMDecode o = do
  encoding <- o .: "encoding"
  return $ M.Decode.apply encoding

parseMSort :: MiddlewareParser
parseMSort o = do
  function <- o .: "function"
  return $ M.Sort.apply function

parseMFilter :: MiddlewareParser
parseMFilter o = do
  predicate <- o .: "operation"
  return $ M.Filter.apply predicate
