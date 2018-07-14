module Follow.Strategies
  ( validStrategies
  , Arguments
  , ArgumentsDSL
  ) where

import           Text.Parsec (Parsec)

type Name = String

data Value
  = VString String
  | VStringList [String]
  deriving (Show, Eq)

type Arguments = [(Name, Value)]

type ArgumentsDSL = [(Name, Parsec String () Value)]

validStrategies :: [String]
validStrategies = ["null"]
