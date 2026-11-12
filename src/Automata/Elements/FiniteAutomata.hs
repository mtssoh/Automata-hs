module Automata.Elements.FiniteAutomata (
  DFA,
  dfaAlphabet,
  dfaDelta,
  dfaStates,
  dfaInitialState,
  dfaFinalStates,
  makeDFA,
  DeterministicDeltaMap (..),
  NFA,
  nfaAlphabet,
  nfaDelta,
  nfaFinalStates,
  nfaInitialState,
  nfaStates,
  makeNFA,
  NonDeterministicDeltaMap (..),
  NFAe,
  nfaeAlphabet,
  nfaeDelta,
  nfaeFinalStates,
  nfaeInitialState,
  nfaeStates,
  makeNFAe,
  NonDeterministicEpsilonDeltaMap (..),
) where

import Automata.Core qualified as Core
import Data.Map qualified as Map
import Data.Set qualified as Set

type AutomataConstructor symbol delta automata =
  Set.Set Core.State ->
  Core.Alphabet symbol ->
  delta ->
  Core.State ->
  Set.Set Core.State ->
  automata

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

validateDeltaElems :: (a -> Set.Set b -> Bool) -> Set.Set b -> [a] -> Bool
validateDeltaElems inclusion set list =
  all (`inclusion` set) list

getDeltaElems :: Map.Map (Core.State, a) b -> ([Core.State], [a], [b])
getDeltaElems transition =
  let inputStates = map fst $ Map.keys transition
      inputSymbols = map snd $ Map.keys transition
      outputStates = Map.elems transition
   in (inputStates, inputSymbols, outputStates)

instance (Ord symbol) => Core.Delta symbol (DeterministicDeltaMap symbol) where
  validateDelta states alphabet (DeterministicDeltaMap transition) =
    all validateStates [inputStates, outputStates] && validAlphabet
   where
    (inputStates, inputSymbols, outputStates) = getDeltaElems transition
    validateStates = validateDeltaElems Set.member states
    validAlphabet = validateDeltaElems Set.member alphabet inputSymbols

instance (Ord symbol) => Core.Delta symbol (NonDeterministicDeltaMap symbol) where
  validateDelta states alphabet (NonDeterministicDeltaMap transition) =
    validInputStates && validOutputStates && validAlphabet
   where
    (inputStates, inputSymbols, outputStates) = getDeltaElems transition
    validInputStates = validateDeltaElems Set.member states inputStates
    validOutputStates = validateDeltaElems Set.isSubsetOf states outputStates
    validAlphabet = validateDeltaElems Set.member alphabet inputSymbols

instance (Ord symbol) => Core.Delta symbol (NonDeterministicEpsilonDeltaMap symbol) where
  validateDelta states alphabet (NonDeterministicEpsilonDeltaMap transition) =
    validInputStates && validOutputStates && validAlphabet
   where
    (inputStates, inputSymbols, outputStates) = getDeltaElems transition
    validInputStates = validateDeltaElems Set.member states inputStates
    validOutputStates = validateDeltaElems Set.isSubsetOf states outputStates
    validAlphabet = all validSymbol inputSymbols
    validSymbol Nothing = True
    validSymbol (Just symbol) = symbol `Set.member` alphabet

data DFA a = DFA
  { dfaStates :: Set.Set Core.State
  , dfaAlphabet :: Core.Alphabet a
  , dfaDelta :: DeterministicDeltaMap a
  , dfaInitialState :: Core.State
  , dfaFinalStates :: Set.Set Core.State
  }

data NFA a = NFA
  { nfaStates :: Set.Set Core.State
  , nfaAlphabet :: Core.Alphabet a
  , nfaDelta :: NonDeterministicDeltaMap a
  , nfaInitialState :: Core.State
  , nfaFinalStates :: Set.Set Core.State
  }

data NFAe a = NFAe
  { nfaeStates :: Set.Set Core.State
  , nfaeAlphabet :: Core.Alphabet a
  , nfaeDelta :: NonDeterministicEpsilonDeltaMap a
  , nfaeInitialState :: Core.State
  , nfaeFinalStates :: Set.Set Core.State
  }

instance Core.Automata (DFA a)
instance Core.Automata (NFA a)
instance Core.Automata (NFAe a)

makeAutomata ::
  (Core.Delta symbol delta, Core.Automata automata) =>
  AutomataConstructor symbol delta automata ->
  Set.Set Core.State ->
  Core.Alphabet symbol ->
  delta ->
  Core.State ->
  Set.Set Core.State ->
  Maybe automata
makeAutomata constructor states alphabet delta initialState finalStates =
  if validStates && validDelta
    then Just (constructor states alphabet delta initialState finalStates)
    else Nothing
 where
  validStates = initialState `Set.member` states && finalStates `Set.isSubsetOf` states
  validDelta = Core.validateDelta states alphabet delta

makeDFA ::
  (Ord a) =>
  Set.Set Core.State ->
  Core.Alphabet a ->
  DeterministicDeltaMap a ->
  Core.State ->
  Set.Set Core.State ->
  Maybe (DFA a)
makeDFA = makeAutomata DFA

makeNFA ::
  (Ord a) =>
  Set.Set Core.State ->
  Core.Alphabet a ->
  NonDeterministicDeltaMap a ->
  Core.State ->
  Set.Set Core.State ->
  Maybe (NFA a)
makeNFA = makeAutomata NFA

makeNFAe ::
  (Ord a) =>
  Set.Set Core.State ->
  Core.Alphabet a ->
  NonDeterministicEpsilonDeltaMap a ->
  Core.State ->
  Set.Set Core.State ->
  Maybe (NFAe a)
makeNFAe = makeAutomata NFAe
