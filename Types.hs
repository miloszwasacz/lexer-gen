module Types
  ( StateName
  , AcceptingStates
  , NFA
  , NFAState
  , NFATransition
  , TransitionType(..)
  , DFA
  , DFAState
  , DFATransition
  , exNFA
  , exNFA2
  ) where

import           Data.Set

type NFA = [NFAState]

type DFA = [(StateName, [DFATransition])]

type NFAState = (StateName, [NFATransition])

type DFAState = (StateName, [DFATransition])

type NFATransition = (TransitionType, StateName)

data TransitionType
  = Epsilon
  | Input !Char

type DFATransition = (Char, StateName)

type StateName = String

type AcceptingStates = Set String


-- Examples
exNFA :: (NFA, AcceptingStates)
exNFA =
  ( [ ("n0", [(Input 'a', "n1")])
    , ("n1", [(Epsilon, "n2")])
    , ("n2", [(Epsilon, "n3"), (Epsilon, "n9")])
    , ("n3", [(Epsilon, "n4"), (Epsilon, "n6")])
    , ("n4", [(Input 'b', "n5")])
    , ("n5", [(Epsilon, "n8")])
    , ("n6", [(Input 'c', "n7")])
    , ("n7", [(Epsilon, "n8")])
    , ("n8", [(Epsilon, "n3"), (Epsilon, "n9")])
    , ("n9", [])
    ]
  , fromList ["n9"])

exNFA2 :: (NFA, AcceptingStates)
exNFA2 =
  ( [ ("n0", [(Epsilon, "n1"), (Epsilon, "n7")])
    , ("n1", [(Epsilon, "n2"), (Epsilon, "n4")])
    , ("n2", [(Input 'a', "n3")])
    , ("n3", [(Epsilon, "n6")])
    , ("n4", [(Input 'b', "n5")])
    , ("n5", [(Epsilon, "n6")])
    , ("n6", [(Epsilon, "n1"), (Epsilon, "n7")])
    , ("n7", [(Input 'a', "n8")])
    , ("n8", [(Input 'b', "n9")])
    , ("n9", [(Input 'b', "n10")])
    , ("n10", [])
    ]
  , fromList ["n10"])
