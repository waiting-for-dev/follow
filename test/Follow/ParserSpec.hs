{-# LANGUAGE OverloadedStrings #-}

module Follow.ParserSpec where

import           Control.Monad.Catch         (MonadThrow)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B (unlines)
import           Data.List                   (isInfixOf)
import           Data.Time                   (LocalTime)
import           Data.Yaml                   (FromJSON, ParseException,
                                              decodeThrow)
import           Follow                      (directoryFromRecipe)
import qualified Follow.Fetchers.WebScraping as F.WebScraping (Selector (..),
                                                               SelectorItem (..))
import qualified Follow.Middlewares.Decode   as M.Decode (Encoding (..))
import qualified Follow.Middlewares.Filter   as M.Filter (Predicate)
import qualified Follow.Middlewares.Sort     as M.Sort (ComparisonFunction)
import           Follow.Parser
import           Follow.Types                (Directory (..), Entry (..),
                                              Fetched, Recipe (..),
                                              Subject (..))
import           Helpers.Factories
import           HTTP.Follow                 (HTTPError (..))
import           Test.Hspec

decode' :: (MonadThrow m, FromJSON a) => ByteString -> m a
decode' = decodeThrow

spec :: Spec
spec = do
  describe ".FromJSON" $ do
    describe "Subject" $ do
      it "has instance" $ do
        let string =
              B.unlines
                ["title: Title", "description: Description", "tags: [tag]"]
        subject <- decode' string :: IO Subject
        subject `shouldBe`
          Subject
            {sTitle = "Title", sDescription = "Description", sTags = ["tag"]}
    describe "SelectorItem" $ do
      it "recognizes Attr constructor" $ do
        let string =
              B.unlines
                ["type: attr", "options:", "  css: .selector", "  name: href"]
        item <- decode' string :: IO F.WebScraping.SelectorItem
        item `shouldBe` F.WebScraping.Attr ".selector" "href"
      it "recognizes InnerText constructor" $ do
        let string = B.unlines ["type: text", "options:", "  css: .selector"]
        item <- decode' string :: IO F.WebScraping.SelectorItem
        item `shouldBe` F.WebScraping.InnerText ".selector"
      it "throws custom error when type is unknown" $ do
        let string = B.unlines ["type: unknown", "options:", "  css: .selector"]
        (decode' string :: IO F.WebScraping.SelectorItem) `shouldThrow` \e ->
          ("Unknown type 'unknown' for selector item" `isInfixOf`
           show (e :: ParseException))
    describe "Selector" $ do
      it "has instance" $ do
        let string =
              B.unlines
                [ "uri:"
                , "  type: text"
                , "  options:"
                , "    css: .uri"
                , "guid:"
                , "  type: attr"
                , "  options:"
                , "    css: .guid"
                , "    name: href"
                , "title: null"
                , "description: null"
                , "author: null"
                , "publish_date: null"
                ]
        selector <- decode' string :: IO F.WebScraping.Selector
        selector `shouldBe`
          F.WebScraping.Selector
            { F.WebScraping.selURI = Just $ F.WebScraping.InnerText ".uri"
            , F.WebScraping.selGUID = Just $ F.WebScraping.Attr ".guid" "href"
            , F.WebScraping.selTitle = Nothing
            , F.WebScraping.selDescription = Nothing
            , F.WebScraping.selAuthor = Nothing
            , F.WebScraping.selPublishDate = Nothing
            }
    describe "Encoding" $ do
      it "recognizes UTF8 constructor" $ do
        let string = "utf8"
        encoding <- decode' string :: IO M.Decode.Encoding
        encoding `shouldBe` M.Decode.UTF8
      it "recognizes UTF16LE constructor" $ do
        let string = "utf16le"
        encoding <- decode' string :: IO M.Decode.Encoding
        encoding `shouldBe` M.Decode.UTF16LE
      it "recognizes UTF16BE constructor" $ do
        let string = "utf16be"
        encoding <- decode' string :: IO M.Decode.Encoding
        encoding `shouldBe` M.Decode.UTF16BE
      it "recognizes UTF32LE constructor" $ do
        let string = "utf32le"
        encoding <- decode' string :: IO M.Decode.Encoding
        encoding `shouldBe` M.Decode.UTF32LE
      it "recognizes UTF32BE constructor" $ do
        let string = "utf32be"
        encoding <- decode' string :: IO M.Decode.Encoding
        encoding `shouldBe` M.Decode.UTF32BE
      it "throws custom error when type is unknown" $ do
        let string = "unknown"
        (decode' string :: IO M.Decode.Encoding) `shouldThrow` \e ->
          ("Unknown type 'unknown' for encoding" `isInfixOf`
           show (e :: ParseException))
    describe "ComparisonFunction" $ do
      it "recognizes byGetter with each field" $ do
        let string = B.unlines ["type: by_field", "options:", "  field: title"]
        function <- decode' string :: IO M.Sort.ComparisonFunction
        let entry1 = _entry {eTitle = Just "A"}
        let entry2 = _entry {eTitle = Just "B"}
        function entry1 entry2 `shouldBe` LT
      it "throws custom error when field is unknown" $ do
        let string =
              B.unlines ["type: by_field", "options:", "  field: unknown"]
        (decode' string :: IO M.Sort.ComparisonFunction) `shouldThrow` \e ->
          ("Unknown field 'unknown' for by field comparison function" `isInfixOf`
           show (e :: ParseException))
      it "throws custom error when type is unknown" $ do
        let string = B.unlines ["type: unknown", "options:", "  field: unknown"]
        (decode' string :: IO M.Sort.ComparisonFunction) `shouldThrow` \e ->
          ("Unknown function 'unknown' for comparison function" `isInfixOf`
           show (e :: ParseException))
    describe "Predicate" $ do
      it "recognizes equalP" $ do
        let string =
              B.unlines
                ["type: equal", "options:", "  field: title", "  value: A"]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "A"}
        predicate entry `shouldBe` True
      it "recognizes lessP" $ do
        let string =
              B.unlines
                ["type: less", "options:", "  field: title", "  value: B"]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "A"}
        predicate entry `shouldBe` True
      it "recognizes greaterP" $ do
        let string =
              B.unlines
                ["type: greater", "options:", "  field: title", "  value: A"]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "B"}
        predicate entry `shouldBe` True
      it "recognizes infixP" $ do
        let string =
              B.unlines
                ["type: infix", "options:", "  field: title", "  value: B"]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "ABC"}
        predicate entry `shouldBe` True
      it "fails with custom error when infixP is given with non text getter" $ do
        let string =
              B.unlines
                [ "type: infix"
                , "options:"
                , "  field: publish_date"
                , "  value: A"
                ]
        (decode' string :: IO M.Filter.Predicate) `shouldThrow` \e ->
          ("Tried to apply 'infix' filter with a field which is not text" `isInfixOf`
           show (e :: ParseException))
      it "recognizes prefixP" $ do
        let string =
              B.unlines
                ["type: prefix", "options:", "  field: title", "  value: A"]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "ABC"}
        predicate entry `shouldBe` True
      it "fails with custom error when prefixP is given with non text getter" $ do
        let string =
              B.unlines
                [ "type: prefix"
                , "options:"
                , "  field: publish_date"
                , "  value: A"
                ]
        (decode' string :: IO M.Filter.Predicate) `shouldThrow` \e ->
          ("Tried to apply 'prefix' filter with a field which is not text" `isInfixOf`
           show (e :: ParseException))
      it "recognizes suffixP" $ do
        let string =
              B.unlines
                ["type: suffix", "options:", "  field: title", "  value: C"]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "ABC"}
        predicate entry `shouldBe` True
      it "fails with custom error when prefixP is given with non text getter" $ do
        let string =
              B.unlines
                [ "type: suffix"
                , "options:"
                , "  field: publish_date"
                , "  value: A"
                ]
        (decode' string :: IO M.Filter.Predicate) `shouldThrow` \e ->
          ("Tried to apply 'suffix' filter with a field which is not text" `isInfixOf`
           show (e :: ParseException))
      it "recognizes notP" $ do
        let string =
              B.unlines
                [ "type: not"
                , "options:"
                , "  operation:"
                , "    type: prefix"
                , "    options:"
                , "      field: title"
                , "      value: D"
                ]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "ABC"}
        predicate entry `shouldBe` True
      it "recognizes andP" $ do
        let string =
              B.unlines
                [ "type: and"
                , "options:"
                , "  operation1:"
                , "    type: prefix"
                , "    options:"
                , "      field: title"
                , "      value: A"
                , "  operation2:"
                , "    type: suffix"
                , "    options:"
                , "      field: title"
                , "      value: C"
                ]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "ABC"}
        predicate entry `shouldBe` True
      it "recognizes orP" $ do
        let string =
              B.unlines
                [ "type: or"
                , "options:"
                , "  operation1:"
                , "    type: prefix"
                , "    options:"
                , "      field: title"
                , "      value: A"
                , "  operation2:"
                , "    type: suffix"
                , "    options:"
                , "      field: title"
                , "      value: C"
                ]
        predicate <- decode' string :: IO M.Filter.Predicate
        let entry = _entry {eTitle = Just "ABD"}
        predicate entry `shouldBe` True
      it "fails with custom error when format for a time field is unknown" $ do
        let string =
              B.unlines
                [ "type: equal"
                , "options:"
                , "  field: publish_date"
                , "  value: unknown"
                ]
        (decode' string :: IO M.Filter.Predicate) `shouldThrow` \e ->
          ("Format for time is unknown" `isInfixOf` show (e :: ParseException))
  describe "Fetcher" $ do
    it "recognizes feed" $ do
      let string = B.unlines ["type: feed", "options:", "  url: invalid"]
      fetched <- decode' string :: IO (Fetched IO)
      fetched `shouldThrow` (== URLWrongFormat)
    it "recognizes web scraping" $ do
      let string =
            B.unlines
              [ "type: webscraping"
              , "options:"
              , "  url: invalid"
              , "  selector:"
              , "    title: null"
              , "    description: null"
              , "    uri: null"
              , "    guid: null"
              , "    author: null"
              , "    publish_date: null"
              ]
      fetched <- decode' string :: IO (Fetched IO)
      fetched `shouldThrow` (== URLWrongFormat)
  describe "Middleware" $ do
    it "recognizes decode" $ do
      let string = B.unlines ["type: decode", "options:", "  encoding: utf8"]
      middleware <- decode' string
      let directory = _directory
      middleware directory `shouldBe` directory
    it "recognizes sort" $ do
      let string =
            B.unlines
              [ "type: sort"
              , "options:"
              , "  function:"
              , "    type: by_field"
              , "    options:"
              , "      field: title"
              ]
      middleware <- decode' string
      let entry1 = _entry {eTitle = Just "B"}
      let entry2 = _entry {eTitle = Just "B"}
      let directory = _directory {dEntries = [entry1, entry2]}
      middleware directory `shouldBe` directory {dEntries = [entry2, entry1]}
    it "recognizes filter" $ do
      let string =
            B.unlines
              [ "type: filter"
              , "options:"
              , "  operation:"
              , "    type: equal"
              , "    options:"
              , "      field: title"
              , "      value: A"
              ]
      middleware <- decode' string
      let entry1 = _entry {eTitle = Just "A"}
      let entry2 = _entry {eTitle = Just "B"}
      let directory = _directory {dEntries = [entry1, entry2]}
      middleware directory `shouldBe` directory {dEntries = [entry1]}
  describe "Recipe" $ do
    it "has instance" $ do
      let string =
            B.unlines
              [ "subject:"
              , "  title: Title"
              , "  description: Description"
              , "  tags: [tag]"
              , "steps:"
              , "  -"
              , "    - type: feed"
              , "      options:"
              , "        url: invalid"
              , "    -"
              , "      - type: sort"
              , "        options:"
              , "          function:"
              , "            type: by_field"
              , "            options:"
              , "              field: title"
              , "middlewares:"
              , "  -"
              , "    type: decode"
              , "    options:"
              , "      encoding: utf8"
              ]
      recipe <- decode' string :: IO (Recipe IO)
      directoryFromRecipe recipe `shouldThrow` (== URLWrongFormat)
