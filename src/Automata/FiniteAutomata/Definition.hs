module Automata.FiniteAutomata.Definition (
  DFA,
  makeDFA,
  dfaStates,
  dfaAlphabet,
  dfaDelta,
  dfaInitialState,
  dfaFinalStates,
  NFA,
  makeNFA,
  nfaStates,
  nfaAlphabet,
  nfaDelta,
  nfaInitialState,
  nfaFinalStates,
  NFAe,
  makeNFAe,
  nfaeStates,
  nfaeAlphabet,
  nfaeDelta,
  nfaeInitialState,
  nfaeFinalStates
) where

import Automata.Core qualified as Core
import Data.Set qualified as Set
import Automata.FiniteAutomata.Delta

type AutomataConstructor symbol delta automata =
  Set.Set Core.State ->
  Core.Alphabet symbol ->
  delta ->
  Core.State ->
  Set.Set Core.State ->
  automata

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
  validDelta = Core.validate states alphabet delta

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
