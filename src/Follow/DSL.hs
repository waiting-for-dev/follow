{- |
Description: Entry point for dealing with DSL parsing.

This module exposes functions within the DSL context meant to be
consumed at a higher level.
-}
module Follow.DSL
  ( parseDSL
  , Recipe(..)
  , ParseResult
  ) where

import           Follow.DSL.Parser
