{- |
Description: Entry point for dealing with DSL definition and parsing.

This module exposes functions within the DSL context meant to be
consumed from the outside for two purposes: parsing recipes and
defining the DSL for concrete fetchers.

Every recipe must begin with a base DSL (defined in
"Follow.DSL.Format.Internal") that looks like the following:

@
VERSION 1.0
TITLE The recipe title
DESCRIPTION The recipe description
TAGS foo, bar
@

Each line is nothing more than a name/value pair. The first word is
the name, and anything until the end of the line is considered the
value.

Different fetchers can define extra name/value pairs that will go in
new lines just after the base DSL. The definition is given as a
`Follow.Fetchers.ArgumentsDSL` data type. See "Follow.Fetchers"
for more details.

For parsing the haskell recipe from the DSL, functions defined in
"Follow.DSL.Parser" (and reexported here) can be used.
-}
module Follow.DSL
  ( module Follow.DSL.Parser
  , module Follow.DSL.Format
  ) where

import           Follow.DSL.Format
import           Follow.DSL.Parser
