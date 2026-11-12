module Automata.Core (State (..), Symbol (..), Alphabet, Delta (..), Automata) where

import qualified Data.Set as Set

data State = State String deriving (Show, Read, Eq, Ord)

data Symbol a = Symbol a deriving (Show, Read, Eq, Ord)

type Alphabet a = Set.Set a

class Delta symbol delta where
  validateDelta :: Set.Set State -> Alphabet symbol -> delta -> Bool

class Automata automata
