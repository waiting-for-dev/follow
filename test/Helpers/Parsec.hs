{-# LANGUAGE FlexibleContexts #-}

module Helpers.Parsec
  ( parse'
  ) where

import           Text.Parsec (parse)

parse' format = parse format "test"
