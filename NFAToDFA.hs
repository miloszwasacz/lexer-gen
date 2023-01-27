module NFAToDFA
  ( convertNFAToDFA
  ) where

import           Control.Monad (join)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Helper        (addToTuple, rmDupl, withIndex)
import           Types

type Done = Set.Set String

type EClosure = [StateName]

type Move = String

type StartingStates = [StateName]


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

assignDFAStates ::
     AcceptingStates
  -> [([StateName], [(Char, [StateName])])]
  -> (DFA, AcceptingStates)
assignDFAStates ass duplNFA = (map assignNames nfa, newASs)
  where
    nfa     = rmDupl duplNFA
    indexed = withIndex nfa
    nameMap =
      Map.fromList $ map (\((states, _), i) -> (states, 'd' : show i)) indexed
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
    newASs =
      Set.fromList . map snd . filter (hasAny ass . fst) $ Map.toList nameMap
    hasAny set [] = False
    hasAny set (x:xs)
      | Set.member x set = True
      | otherwise        = hasAny set xs

convertNFAToDFA :: (NFA, AcceptingStates) -> (DFA, AcceptingStates)
convertNFAToDFA (nfa, ass) =
  assignDFAStates ass . fst $ groupNFAStates nfa done startStates
  where
    startStates = findStartStates nfa
    done        = Set.fromList startStates


-- Helper
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
