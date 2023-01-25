import           Control.Monad (join)
import qualified Data.Map      as Map
import qualified Data.Set      as Set

type NFA = [NFAState]

type DFA = [(StateName, [DFATransition])]

type NFAState = (StateName, [NFATransition])

type DFAState = (StateName, [DFATransition])

type StateName = String

type NFATransition = (TransitionType, StateName)

data TransitionType
  = Epsilon
  | Input !Char

type DFATransition = (Char, StateName)

type Done = Set.Set String

type EClosure = [StateName]

type Move = String

type StartingStates = [StateName]


-- Examples
exNFA :: NFA
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

exNFA2 :: NFA
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


-- Epsilon-closure
epsClosure :: NFA -> Move -> NFAState -> [StateName]
epsClosure graph move (name, transitions) =
  case filterMoveOrEpsilon move transitions of
    [] -> []
    targets -> names ++ join (map (epsClosure graph newMove) newStates)
      where usedMove = not $ all isEpsilon targets
            newMove =
              if usedMove
                then ""
                else move
            newStates = map (findState graph) names
            names = map snd targets

filterMoveOrEpsilon :: Move -> [NFATransition] -> [NFATransition]
filterMoveOrEpsilon "" = filter isEpsilon
filterMoveOrEpsilon [m] = filter canMove
  where
    canMove (Epsilon, _) = True
    canMove (Input c, _) = m == c
filterMoveOrEpsilon _    = error "Move has to be 1 character!"

isEpsilon :: NFATransition -> Bool
isEpsilon (Epsilon, _) = True
isEpsilon _            = False

isInputState :: NFAState -> Bool
isInputState (_, trans) = not $ all isEpsilon trans


-- NFA to DFA
singleMoveEpsClosure :: NFA -> Char -> [StateName] -> Done -> (EClosure, Done)
singleMoveEpsClosure graph move stateNames done = (newStates, newDone)
  where
    states    = filter isInputState $ map (findState graph) stateNames
    newStates = rmDupl $ concatMap (epsClosure graph [move]) states
    newDone   = foldr Set.insert done newStates

stateEpsClosure :: NFA -> [StateName] -> Done -> ([(Char, EClosure)], Done)
stateEpsClosure graph [] done = ([], done)
stateEpsClosure graph names done = (closures, newDone)
  where
    chars = getChars $ map (findState graph) names
    results =
      map (\c -> addToTuple (singleMoveEpsClosure graph c names done) c) chars
    closures = map (\(cl, _, c) -> (c, cl)) results
    newDone = foldr (Set.union . (\(_, d, _) -> d)) Set.empty results

groupNFAStates ::
     NFA
  -> Done
  -> StartingStates
  -> ([([StateName], [(Char, [StateName])])], Done)
groupNFAStates graph startDone names =
  case stateEpsClosure graph names startDone of
    (moves, done) -> ((names, moves) : nextStates, newDone)
      where (nextStates, newDone) = next done $ map snd moves
  where
    stateSet = Set.fromList $ map fst graph
    next done newStates =
      if stateSet == startDone
        then ([], startDone)
        else (join (map fst new), foldr (Set.union . snd) Set.empty new)
      where
        new = map (groupNFAStates graph done) $ filter (/= names) newStates

assignDFAStates :: [([StateName], [(Char, [StateName])])] -> DFA
assignDFAStates duplNFA = map assignNames nfa
  where
    nfa                  = rmDupl duplNFA
    assignIndex [] _     = []
    assignIndex (s:ss) i = addToTuple s i : assignIndex ss (i + 1)
    indexed              = assignIndex nfa 0
    nameMap =
      Map.fromList $ map (\(states, _, i) -> (states, 'd' : show i)) indexed
    assignNames (states, edges) =
      case Map.lookup states nameMap of
        Just dfaName -> (dfaName, map assignTarget edges)
        Nothing ->
          error $
          "States " ++
          show states ++ " are not in the DFA State Map!\nMap: " ++ show nameMap
    assignTarget (move, target) =
      case Map.lookup target nameMap of
        Just dfaName -> (move, dfaName)
        Nothing ->
          error $
          "Target states " ++
          show target ++ " are not in the DFA State Map!\nMap: " ++ show nameMap

convertNFAToDFA :: NFA -> DFA
convertNFAToDFA nfa =
  assignDFAStates . fst $ groupNFAStates nfa done startStates
  where
    startStates = findStartStates nfa
    done        = Set.fromList startStates


-- Helper (FA)
findState :: NFA -> StateName -> NFAState
findState [] query = error $ "State \"" ++ query ++ "\" not found"
findState ((stateName, transitions):states) query
  | stateName == query = (stateName, transitions)
  | otherwise          = findState states query

findStartStates :: NFA -> [StateName]
findStartStates [] = []
findStartStates fa = rmDupl $ name : epsClosure fa "" s
  where
    (s:ss)    = fa
    (name, _) = s

getChars :: [NFAState] -> [Char]
getChars [] = []
getChars ((_, t):ss) = rmDupl $ foldr inputs [] t ++ getChars ss
  where
    inputs (Input c, _) acc = c : acc
    inputs (Epsilon, _) acc = acc


-- Helper (generic)
rmDupl :: Ord a => [a] -> [a]
rmDupl = Set.toList . Set.fromList

addToTuple (x, y) z = (x, y, z)

printList [] = return ()
printList (x:xs) = do
  print x
  printList xs

-- >>> assignDFAStates $ groupNFAStates exNFA ["n0"]
