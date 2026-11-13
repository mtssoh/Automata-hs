module Automata.FiniteAutomata.Delta (
  DeterministicDeltaMap (..),
  NonDeterministicDeltaMap (..),
  NonDeterministicEpsilonDeltaMap (..),
) where

import Automata.Core qualified as Core
import Data.Map qualified as Map
import Data.Set qualified as Set

type DeltaMap symbol transition =
  Map.Map (Core.State, symbol) transition

newtype DeterministicDeltaMap symbol
  = DeterministicDeltaMap (DeltaMap symbol Core.State)
  deriving (Show, Read)

newtype NonDeterministicDeltaMap symbol
  = NonDeterministicDeltaMap (DeltaMap symbol (Set.Set Core.State))
  deriving (Show, Read)

newtype NonDeterministicEpsilonDeltaMap symbol
  = NonDeterministicEpsilonDeltaMap (DeltaMap (Maybe symbol) (Set.Set Core.State))
  deriving (Show, Read)

validateElems :: (a -> Set.Set b -> Bool) -> Set.Set b -> [a] -> Bool
validateElems inclusion set list =
  all (`inclusion` set) list

getDeltaElems :: Map.Map (Core.State, a) b -> ([Core.State], [a], [b])
getDeltaElems transition =
  let inputStates = map fst $ Map.keys transition
      inputSymbols = map snd $ Map.keys transition
      outputStates = Map.elems transition
   in (inputStates, inputSymbols, outputStates)

instance (Ord symbol) => Core.Delta symbol (DeterministicDeltaMap symbol) where
  type Input (DeterministicDeltaMap symbol) = (Core.State, symbol)
  type Output (DeterministicDeltaMap symbol) = Maybe Core.State
  validate states alphabet (DeterministicDeltaMap transition) =
    all validateStates [inputStates, outputStates] && validAlphabet
   where
    (inputStates, inputSymbols, outputStates) = getDeltaElems transition
    validateStates = validateElems Set.member states
    validAlphabet = validateElems Set.member alphabet inputSymbols
  apply (DeterministicDeltaMap delta) (state, symbol) =
    Map.lookup (state, symbol) delta

instance (Ord symbol) => Core.Delta symbol (NonDeterministicDeltaMap symbol) where
  type Input (NonDeterministicDeltaMap symbol) = (Core.State, symbol)
  type Output (NonDeterministicDeltaMap symbol) = Set.Set Core.State
  validate states alphabet (NonDeterministicDeltaMap transition) =
    validInputStates && validOutputStates && validAlphabet
   where
    (inputStates, inputSymbols, outputStates) = getDeltaElems transition
    validInputStates = validateElems Set.member states inputStates
    validOutputStates = validateElems Set.isSubsetOf states outputStates
    validAlphabet = validateElems Set.member alphabet inputSymbols
  apply (NonDeterministicDeltaMap delta) (state, symbol) =
    case Map.lookup (state, symbol) delta of
      Just stateSet -> stateSet
      Nothing -> Set.empty

instance (Ord symbol) => Core.Delta symbol (NonDeterministicEpsilonDeltaMap symbol) where
  type Input (NonDeterministicEpsilonDeltaMap symbol) = (Core.State, Maybe symbol)
  type Output (NonDeterministicEpsilonDeltaMap symbol) = Set.Set Core.State
  validate states alphabet (NonDeterministicEpsilonDeltaMap transition) =
    validInputStates && validOutputStates && validAlphabet
   where
    (inputStates, inputSymbols, outputStates) = getDeltaElems transition
    validInputStates = validateElems Set.member states inputStates
    validOutputStates = validateElems Set.isSubsetOf states outputStates
    validAlphabet = all validSymbol inputSymbols
    validSymbol Nothing = True
    validSymbol (Just symbol) = symbol `Set.member` alphabet
  apply (NonDeterministicEpsilonDeltaMap delta) (state, symbol) =
    case Map.lookup (state, symbol) delta of
      Just stateSet -> stateSet
      Nothing -> Set.empty
