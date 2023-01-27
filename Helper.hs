module Helper where

import qualified Data.Set as Set

split f xs = [filter (not . f) xs, filter f xs]

rmDupl :: Ord a => [a] -> [a]
rmDupl = Set.toList . Set.fromList

addToTuple (x, y) z = (x, y, z)

withIndex = withIndex' 0

withIndex' _ []     = []
withIndex' i (x:xs) = (x, i) : withIndex' (i + 1) xs

setContains :: Ord a => Set.Set a -> a -> Bool
setContains = flip Set.member

printList [] = return ()
printList (x:xs) = do
  print x
  printList xs
