{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Hoist not" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Hopcroft
  ( minimize
  ) where

import           Control.Monad (join)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Helper        (rmDupl, setContains, split, withIndex)
import           Types         (AcceptingStates, DFA, DFAState, StateName)

splitByAccepting :: (DFA, AcceptingStates) -> [[DFAState]]
splitByAccepting (dfa, ass) = mapToStates $ split (setContains ass) names
  where
    names       = map fst dfa
    mapToStates = map (map (findState dfa))

splitByMovesSingle :: [DFAState] -> [[DFAState]]
splitByMovesSingle = foldr addToGroup []
  where
    addToGroup s []       = [[s]]
    addToGroup s ([]:xss) = error "Internal error (empty list)"
    addToGroup s ((x:xs):xss)
      | snd s == snd x = (s : x : xs) : xss
      | otherwise      = (x : xs) : addToGroup s xss

splitByMoves :: [[DFAState]] -> [[DFAState]]
splitByMoves ss
  | ss == newSs = newSs
  | otherwise   = splitByMoves newSs
  where
    newSs = rmDupl . join $ map splitByMovesSingle ss

minimize :: (DFA, AcceptingStates) -> (DFA, AcceptingStates)
minimize (dfa, as) = rename minDFA as groups
  where
    groups = splitByMoves $ splitByAccepting (dfa, as)
    minDFA = map head groups

rename :: DFA -> AcceptingStates -> [[DFAState]] -> (DFA, AcceptingStates)
rename dfa as groups = (updateDFA dfa, updateAS as)
  where
    indexed = join . map lift . withIndex $ map (map fst) groups
    nameMap = Map.fromList $ map (\(name, i) -> (name, 'd' : show i)) indexed
    updateDFA =
      map (\(name, trans) -> (replaceName name, map replaceTarget trans))
    updateAS = Set.map replaceName
    replaceName n =
      case Map.lookup n nameMap of
        Just new -> new
        Nothing ->
          error $
          "State " ++
          show n ++ " is not in the DFA State Map!\nMap: " ++ show nameMap
    replaceTarget (c, n) =
      case Map.lookup n nameMap of
        Just new -> (c, new)
        Nothing ->
          error $
          "Target state " ++
          show n ++ " is not in the DFA State Map!\nMap: " ++ show nameMap
    lift (xs, n) = map (\x -> (x, n)) xs

findState :: DFA -> StateName -> DFAState
findState [] query = error $ "State \"" ++ query ++ "\" not found"
findState ((stateName, transitions):states) query
  | stateName == query = (stateName, transitions)
  | otherwise          = findState states query
