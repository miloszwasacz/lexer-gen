import           Control.Monad (join)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Lex

f .> g = g f

type Done = Set.Set String

type EClosure = [StateName]

type Move = String

step :: FA -> Char -> [StateName] -> Done -> (EClosure, Done)
step graph move stateNames done = (newStates, newDone)
  where
    states    = filter isInputState $ map (findState graph) stateNames
    newStates = rmDupl $ concatMap (epsClosure graph [move]) states
    newDone   = foldr Set.insert done newStates

epsClosure :: FA -> Move -> State -> [StateName]
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

filterMoveOrEpsilon :: Move -> [Transition] -> [Transition]
filterMoveOrEpsilon "" = filter isEpsilon
filterMoveOrEpsilon [m] = filter canMove
  where
    canMove (Epsilon, _) = True
    canMove (Input c, _) = m == c
filterMoveOrEpsilon _    = error "Move has to be 1 character!"

isEpsilon :: Transition -> Bool
isEpsilon (Epsilon, _) = True
isEpsilon _            = False

isInputState :: State -> Bool
isInputState (_, trans) = not $ all isEpsilon trans

step' :: FA -> [StateName] -> Done -> ([(EClosure, Char)], Done)
step' graph [] done = ([], done)
step' graph names done = (closures, newDone)
  where
    chars    = getChars $ map (findState graph) names
    results  = map (\c -> addToTuple (step graph c names done) c) chars
    closures = map (\(cl, _, c) -> (cl, c)) results
    newDone  = foldr (Set.union . (\(_, d, _) -> d)) Set.empty results

getChars :: [State] -> [Char]
getChars [] = []
getChars ((_, t):ss) = rmDupl $ foldr inputs [] t ++ getChars ss
  where
    inputs (Input c, _) acc = c : acc
    inputs (Epsilon, _) acc = acc

type StartingStates = [StateName]

groupNFAStates ::
     FA
  -> Done
  -> StartingStates
  -> ([([StateName], [(Char, [StateName])])], Done)
groupNFAStates graph startDone names =
  case step' graph names startDone of
    (moves, done) ->
      ((names, map (\(t, c) -> (c, t)) moves) : nextStates, newDone)
      where (nextStates, newDone) = next done $ map fst moves
  where
    stateSet = Set.fromList $ map fst graph
    next done newStates =
      if stateSet == startDone
        then ([], startDone)
        else (join (map fst new), foldr (Set.union . snd) Set.empty new)
      where
        new = map (groupNFAStates graph done) $ filter (/= names) newStates

addToTuple (x, y) z = (x, y, z)

type DFA = [(StateName, [(Char, StateName)])]

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

findStartStates :: FA -> [StateName]
findStartStates [] = []
findStartStates fa = rmDupl $ name : epsClosure fa "" s
  where
    (s:ss)    = fa
    (name, _) = s

convertNFAToDFA :: FA -> DFA
convertNFAToDFA nfa =
  assignDFAStates . fst $ groupNFAStates nfa done startStates
  where
    startStates = findStartStates nfa
    done        = Set.fromList startStates

convertNFAToDFA' nfa =
  Map.fromList $
  map (\(states, _, i) -> (states, 'd' : show i)) $
  assignIndex (rmDupl . fst $ groupNFAStates nfa done startStates) 0
  where
    startStates          = findStartStates nfa
    done                 = Set.fromList startStates
    assignIndex [] _     = []
    assignIndex (s:ss) i = addToTuple s i : assignIndex ss (i + 1)

printList [] = return ()
printList (x:xs) = do
  print x
  printList xs

-- >>> assignDFAStates $ groupNFAStates exNFA ["n0"]
