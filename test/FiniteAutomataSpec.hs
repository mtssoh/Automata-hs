module FiniteAutomataSpec (tests) where

import Automata.Core qualified as Core
import Automata.FiniteAutomata.Definition
import Automata.FiniteAutomata.Delta
import Data.Map qualified as Map
import Data.Set qualified as Set
import Test.HUnit

isValid :: Maybe a -> Bool
isValid (Just _) = True
isValid Nothing = False

-- Estados

q0, q2, q3 :: Core.State
q0 = Core.State "q0"
q2 = Core.State "q2"
q3 = Core.State "q3"

states1, states2 :: Set.Set Core.State
states1 = Set.fromList [q0]
states2 = Set.fromList [q0, q2, q3]

-- Alfabetos

alphabet1 :: Core.Alphabet String
alphabet1 = Set.fromList ["1", "2", "3"]

alphabet2 :: Core.Alphabet Int
alphabet2 = Set.fromList [1, 2, 3, 4, 5]

-- DFA

validDFA :: Maybe (DFA String)
validDFA =
  makeDFA
    states2
    alphabet1
    ( DeterministicDeltaMap $
        Map.fromList
          [ ((q0, "1"), q2)
          , ((q2, "2"), q3)
          , ((q3, "3"), q0)
          ]
    )
    q0
    (Set.singleton q3)

invalidAlphabetDFA :: Maybe (DFA String)
invalidAlphabetDFA =
  makeDFA
    states2
    (Set.fromList ["1", "2"])
    ( DeterministicDeltaMap $
        Map.singleton (q0, "3") q2
    )
    q0
    (Set.singleton q2)

invalidStateDFA :: Maybe (DFA String)
invalidStateDFA =
  makeDFA
    states1
    alphabet1
    ( DeterministicDeltaMap $
        Map.singleton (q0, "1") q2
    )
    q0
    (Set.singleton q0)

-- NFA

validNFA :: Maybe (NFA Int)
validNFA =
  makeNFA
    states2
    alphabet2
    ( NonDeterministicDeltaMap $
        Map.fromList
          [ ((q0, 1), Set.fromList [q0, q2])
          , ((q2, 2), Set.singleton q3)
          ]
    )
    q0
    (Set.singleton q3)

invalidAlphabetNFA :: Maybe (NFA Int)
invalidAlphabetNFA =
  makeNFA
    states2
    (Set.fromList [1, 2, 3, 4])
    ( NonDeterministicDeltaMap $
        Map.singleton (q0, 5) (Set.singleton q2)
    )
    q0
    (Set.singleton q2)

invalidStateNFA :: Maybe (NFA Int)
invalidStateNFA =
  makeNFA
    states1
    alphabet2
    ( NonDeterministicDeltaMap $
        Map.singleton (q0, 1) (Set.fromList [q0, q3])
    )
    q0
    (Set.singleton q0)

-- NFAe

validNFAe :: Maybe (NFAe String)
validNFAe =
  makeNFAe
    states2
    alphabet1
    ( NonDeterministicEpsilonDeltaMap $
        Map.fromList
          [ ((q0, Nothing), Set.singleton q2)
          , ((q2, Just "1"), Set.singleton q3)
          ]
    )
    q0
    (Set.singleton q3)

invalidAlphabetNFAe :: Maybe (NFAe String)
invalidAlphabetNFAe =
  makeNFAe
    states2
    (Set.fromList ["1", "2"])
    ( NonDeterministicEpsilonDeltaMap $
        Map.fromList
          [ ((q0, Nothing), Set.singleton q2)
          , ((q2, Just "3"), Set.singleton q3)
          ]
    )
    q0
    (Set.singleton q3)

invalidStateNFAe :: Maybe (NFAe String)
invalidStateNFAe =
  makeNFAe
    states1
    alphabet1
    ( NonDeterministicEpsilonDeltaMap $
        Map.singleton (q0, Nothing) (Set.singleton q2)
    )
    q0
    (Set.singleton q0)

-- Tests de apply

applyTests :: Test
applyTests =
  TestList
    [ -- DFA
      "DFA: transicion q0 con 1"
        ~: Just q2
        ~=? ( validDFA >>= \dfa ->
                Core.apply (dfaDelta dfa) (q0, "1")
            )
    , "DFA: transicion q2 con 2"
        ~: Just q3
        ~=? ( validDFA >>= \dfa ->
                Core.apply (dfaDelta dfa) (q2, "2")
            )
    , "DFA: transicion inexistente"
        ~: Nothing
        ~=? ( validDFA >>= \dfa ->
                Core.apply (dfaDelta dfa) (q0, "2")
            )
    , -- NFA
      "NFA: multiples estados destino"
        ~: Just (Set.fromList [q0, q2])
        ~=? ( validNFA >>= \nfa ->
                Just $ Core.apply (nfaDelta nfa) (q0, 1)
            )
    , "NFA: un estado destino"
        ~: Just (Set.singleton q3)
        ~=? ( validNFA >>= \nfa ->
                Just $ Core.apply (nfaDelta nfa) (q2, 2)
            )
    , "NFA: transicion inexistente"
        ~: Just Set.empty
        ~=? ( validNFA >>= \nfa ->
                Just $ Core.apply (nfaDelta nfa) (q0, 3)
            )
    , -- NFA epsilon
      "NFAe: transicion epsilon"
        ~: Just (Set.singleton q2)
        ~=? ( validNFAe >>= \nfa ->
                Just $ Core.apply (nfaeDelta nfa) (q0, Nothing)
            )
    , "NFAe: transicion con simbolo"
        ~: Just (Set.singleton q3)
        ~=? ( validNFAe >>= \nfa ->
                Just $ Core.apply (nfaeDelta nfa) (q2, Just "1")
            )
    , "NFAe: transicion inexistente"
        ~: Just Set.empty
        ~=? ( validNFAe >>= \nfa ->
                Just $ Core.apply (nfaeDelta nfa) (q0, Just "2")
            )
    ]

-- Tests

validateTests :: Test
validateTests =
  TestList
    [ "DFA valido"
        ~: assertBool "Debe aceptar el DFA" (isValid validDFA)
    , "DFA alfabeto invalido"
        ~: assertBool "Debe rechazar simbolo" (not $ isValid invalidAlphabetDFA)
    , "DFA estado invalido"
        ~: assertBool "Debe rechazar estado destino" (not $ isValid invalidStateDFA)
    , "NFA valido"
        ~: assertBool "Debe aceptar el NFA" (isValid validNFA)
    , "NFA alfabeto invalido"
        ~: assertBool "Debe rechazar simbolo" (not $ isValid invalidAlphabetNFA)
    , "NFA estado invalido"
        ~: assertBool "Debe rechazar estados destino" (not $ isValid invalidStateNFA)
    , "NFAe valido"
        ~: assertBool "Debe aceptar epsilon" (isValid validNFAe)
    , "NFAe alfabeto invalido"
        ~: assertBool "Debe rechazar simbolo" (not $ isValid invalidAlphabetNFAe)
    , "NFAe estado invalido"
        ~: assertBool "Debe rechazar destino epsilon" (not $ isValid invalidStateNFAe)
    ]

tests :: Test
tests =
  TestList
    [ "Validacion" ~: validateTests
    , "Aplicacion de delta" ~: applyTests
    ]
