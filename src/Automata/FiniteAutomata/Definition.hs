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
  nfaeFinalStates,
) where

import Automata.Core qualified as Core
import Automata.FiniteAutomata.Delta
import Data.Set qualified as Set

type AutomataConstructor symbol delta state automata =
  Set.Set (Core.State state) ->
  Core.Alphabet symbol ->
  delta ->
  Core.State state ->
  Set.Set (Core.State state) ->
  automata

data DFA a b = DFA
  { dfaStates :: Set.Set (Core.State b)
  , dfaAlphabet :: Core.Alphabet a
  , dfaDelta :: DeterministicDeltaMap a b
  , dfaInitialState :: Core.State b
  , dfaFinalStates :: Set.Set (Core.State b)
  }

data NFA a b = NFA
  { nfaStates :: Set.Set (Core.State b)
  , nfaAlphabet :: Core.Alphabet a
  , nfaDelta :: NonDeterministicDeltaMap a b
  , nfaInitialState :: Core.State b
  , nfaFinalStates :: Set.Set (Core.State b)
  }

data NFAe a b = NFAe
  { nfaeStates :: Set.Set (Core.State b)
  , nfaeAlphabet :: Core.Alphabet a
  , nfaeDelta :: NonDeterministicEpsilonDeltaMap a b
  , nfaeInitialState :: Core.State b
  , nfaeFinalStates :: Set.Set (Core.State b)
  }

instance (Ord a, Ord b) => Core.Automata (DFA a b)
instance (Ord a, Ord b) => Core.Automata (NFA a b)
instance (Ord a, Ord b) => Core.Automata (NFAe a b)

makeAutomata ::
  (Core.Delta symbol delta state, Core.Automata automata, Ord state) =>
  AutomataConstructor symbol delta state automata ->
  Set.Set (Core.State state) ->
  Core.Alphabet symbol ->
  delta ->
  Core.State state ->
  Set.Set (Core.State state) ->
  Maybe automata
makeAutomata constructor states alphabet delta initialState finalStates =
  if validStates && validDelta
    then Just (constructor states alphabet delta initialState finalStates)
    else Nothing
 where
  validStates = initialState `Set.member` states && finalStates `Set.isSubsetOf` states
  validDelta = Core.validate states alphabet delta

makeDFA ::
  (Ord a, Ord b) =>
  Set.Set (Core.State b) ->
  Core.Alphabet a ->
  DeterministicDeltaMap a b ->
  Core.State b ->
  Set.Set (Core.State b) ->
  Maybe (DFA a b)
makeDFA = makeAutomata DFA

makeNFA ::
  (Ord a, Ord b) =>
  Set.Set (Core.State b)->
  Core.Alphabet a ->
  NonDeterministicDeltaMap a b ->
  Core.State b ->
  Set.Set (Core.State b) ->
  Maybe (NFA a b)
makeNFA = makeAutomata NFA

makeNFAe ::
  (Ord a, Ord b) =>
  Set.Set (Core.State b) ->
  Core.Alphabet a ->
  NonDeterministicEpsilonDeltaMap a b ->
  Core.State b ->
  Set.Set (Core.State b) ->
  Maybe (NFAe a b)
makeNFAe = makeAutomata NFAe
