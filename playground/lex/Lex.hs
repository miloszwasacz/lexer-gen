module Lex
  ( FA
  , State
  , StateName
  , Transition
  , TransitionType(..)
  , findState
  , exNFA
  , exNFA2
  , rmDupl
  ) where

import           Control.Monad                   (join)
import qualified Control.Monad.Trans.Writer.Lazy as Wr
import qualified Data.Set                        as Set

data TransitionType
  = Epsilon
  | Input !Char

type Transition = (TransitionType, StateName)

type StateName = String

type State = (StateName, [Transition])

type Alphabet = [String]

type FA = [State]

printFA :: FA -> IO ()
printFA [] = return ()
printFA ((name, targets):fa) = do
  putStrLn $ name ++ " [" ++ showTransitions targets ++ "]"
  printFA fa
  where
    showTransitions []         = ""
    showTransitions [t]        = showTran t
    showTransitions (t:ts)     = showTran t ++ ", " ++ showTransitions ts
    showTran (Epsilon, target) = "---> " ++ target
    showTran (Input c, target) = '-' : c : "-> " ++ target

alAB = ["a", "b"]

alABC = ["a", "b", "c"]

exNFA :: FA
exNFA =
  [ ("n0", [(Input 'a', "n1")])
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

exNFA2 :: FA
exNFA2 =
  [ ("n0", [(Epsilon, "n1"), (Epsilon, "n7")])
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

epsClosure :: FA -> String -> State -> [StateName]
epsClosure _ _ (_, []) = []
epsClosure states move (start, [(Input c, target)])
  | [c] == move = target : epsClos states target
  | otherwise   = []
epsClosure states "" (start, targets) =
  start : join (map (traverseEpsilon states) targets)
epsClosure _ _ _ = error "Invalid NFA"

traverseEpsilon :: FA -> Transition -> [StateName]
traverseEpsilon states (Epsilon, target) =
  target : (join . map (traverseEpsilon states) $ newTargets target)
  where
    newTargets target          = snd $ findState states target
traverseEpsilon _ (Input _, _) = []

epsClos states =
  rmDupl . join . map (traverseEpsilon states) . snd . findState states

findState :: FA -> StateName -> State
findState [] query = error $ "State \"" ++ query ++ "\" not found"
findState ((stateName, transitions):states) query
  | stateName == query = (stateName, transitions)
  | otherwise          = findState states query

getNonEpsilonStates :: FA -> [StateName] -> [State]
getNonEpsilonStates states = filter nonEpsilon . map (findState states)
  where
    nonEpsilon (_, [(Input _, _)]) = True
    nonEpsilon _                   = False

rmDupl :: Ord a => [a] -> [a]
rmDupl = Set.toList . Set.fromList

findStates nfa = map $ findState nfa

getStartStates :: FA -> [State]
getStartStates [] = []
getStartStates ((name, [(Input c, target)]):_) = [(name, [(Input c, target)])]
getStartStates nfa = map (findState nfa) . epsClosure nfa "" $ head nfa

-- >>> epsClosure exampleNFA "a" $ head exampleNFA
-- ["n1","n2","n3","n4","n6","n9"]

-- >>> Set.toList . Set.fromList . join . map (epsClosure exampleNFA "b") $ getNonEpsilonStates exampleNFA ["n1","n2","n3","n4","n6","n9"]
-- ["n3","n4","n5","n6","n8","n9"]

-- >>> Set.toList . Set.fromList . join . map (epsClosure exampleNFA "c") $ getNonEpsilonStates exampleNFA ["n1","n2","n3","n4","n6","n9"]
-- ["n3","n4","n6","n7","n8","n9"]

-- >>> Set.toList . Set.fromList . join . map (epsClosure exampleNFA "b") $ getNonEpsilonStates exampleNFA ["n3","n4","n5","n6","n8","n9"]
-- ["n3","n4","n5","n6","n8","n9"]

-- >>> Set.toList . Set.fromList . join . map (epsClosure exampleNFA "c") $ getNonEpsilonStates exampleNFA ["n3","n4","n6","n7","n8","n9"]
-- ["n3","n4","n6","n7","n8","n9"]
