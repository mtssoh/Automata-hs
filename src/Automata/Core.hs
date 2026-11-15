module Automata.Core (State (..), Symbol (..), Alphabet, Delta (..), Automata) where

import Data.Set qualified as Set

data State a = State a deriving (Show, Read, Eq, Ord)

data Symbol a = Symbol a deriving (Show, Read, Eq, Ord)

type Alphabet a = Set.Set a

class Delta symbol delta state | delta -> symbol, delta -> state where
  type Input delta
  type Output delta

  validate :: Set.Set (State state) -> Alphabet symbol -> delta -> Bool
  apply :: delta -> Input delta -> Output delta

class Automata automata
