module Automata.FiniteAutomata.Delta (
  DeterministicDeltaMap (..),
  NonDeterministicDeltaMap (..),
  NonDeterministicEpsilonDeltaMap (..),
) where

import Automata.Core qualified as Core
import Data.Map qualified as Map
import Data.Set qualified as Set

type DeltaMap symbol state transition =
  Map.Map (Core.State state, symbol) transition

newtype DeterministicDeltaMap symbol state
  = DeterministicDeltaMap (DeltaMap symbol state (Core.State state))
  deriving (Show, Read)

newtype NonDeterministicDeltaMap symbol state
  = NonDeterministicDeltaMap (DeltaMap symbol state (Set.Set (Core.State state)))
  deriving (Show, Read)

newtype NonDeterministicEpsilonDeltaMap symbol state
  = NonDeterministicEpsilonDeltaMap (DeltaMap (Maybe symbol) state (Set.Set (Core.State state)))
  deriving (Show, Read)

validateElems :: (a -> Set.Set b -> Bool) -> Set.Set b -> [a] -> Bool
validateElems inclusion set list =
  all (`inclusion` set) list

getDeltaElems :: Map.Map (Core.State state, a) b -> ([Core.State state], [a], [b])
getDeltaElems transition =
  let inputStates = map fst $ Map.keys transition
      inputSymbols = map snd $ Map.keys transition
      outputStates = Map.elems transition
   in (inputStates, inputSymbols, outputStates)

instance (Ord symbol, Ord state) => Core.Delta symbol (DeterministicDeltaMap symbol state) state where
  type Input (DeterministicDeltaMap symbol state) = (Core.State state, symbol)
  type Output (DeterministicDeltaMap symbol state) = Maybe (Core.State state)
  validate states alphabet (DeterministicDeltaMap transition) =
    all validateStates [inputStates, outputStates] && validAlphabet
   where
    (inputStates, inputSymbols, outputStates) = getDeltaElems transition
    validateStates = validateElems Set.member states
    validAlphabet = validateElems Set.member alphabet inputSymbols
  apply (DeterministicDeltaMap delta) (state, symbol) =
    Map.lookup (state, symbol) delta

instance (Ord symbol, Ord state) => Core.Delta symbol (NonDeterministicDeltaMap symbol state) state where
  type Input (NonDeterministicDeltaMap symbol state) = (Core.State state, symbol)
  type Output (NonDeterministicDeltaMap symbol state) = Set.Set (Core.State state)
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

instance (Ord symbol, Ord state) => Core.Delta symbol (NonDeterministicEpsilonDeltaMap symbol state) state where
  type Input (NonDeterministicEpsilonDeltaMap symbol state) = (Core.State state, Maybe symbol)
  type Output (NonDeterministicEpsilonDeltaMap symbol state) = Set.Set (Core.State state)
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
