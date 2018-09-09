{-|
Description: Decodes entry fields to some encoding.

Middleware to decode entry fields, usually fetched from some external
source, from a given encoding different to default Latin1.

@
import Follow.Middlewares.Decode

apply UTF8 directory
@
-}
module Follow.Middlewares.Decode
  ( apply
  , Encoding(..)
  ) where

import qualified Data.ByteString.Char8    as BSC (pack)
import qualified Data.Text                as T (unpack)
import           Data.Text.Encoding       (decodeUtf16BEWith, decodeUtf16LEWith,
                                           decodeUtf32BEWith, decodeUtf32LEWith,
                                           decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Follow.Types             (Directory (..), Entry (..),
                                           Middleware)

-- | Supported encodings.
data Encoding
  = UTF8
  | UTF16LE
  | UTF16BE
  | UTF32LE
  | UTF32BE

-- | Middleware operation. Give it a `Encoding` and get back the
-- decoded directory.
apply :: Encoding -> Middleware
apply encoding directory =
  directory {dEntries = decodeEntry encoding <$> dEntries directory}

decodeEntry :: Encoding -> Entry -> Entry
decodeEntry encoding entry =
  let d = decodingFunction encoding lenientDecode . BSC.pack . T.unpack
   in entry
        { eURI = d <$> eURI entry
        , eGUID = d <$> eGUID entry
        , eTitle = d <$> eTitle entry
        , eDescription = d <$> eDescription entry
        , eAuthor = d <$> eAuthor entry
        }
  where
    decodingFunction UTF8    = decodeUtf8With
    decodingFunction UTF16LE = decodeUtf16LEWith
    decodingFunction UTF16BE = decodeUtf16BEWith
    decodingFunction UTF32LE = decodeUtf32LEWith
    decodingFunction UTF32BE = decodeUtf32BEWith
