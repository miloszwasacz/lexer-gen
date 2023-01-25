module Helper where

import qualified Data.Set as Set

rmDupl :: Ord a => [a] -> [a]
rmDupl = Set.toList . Set.fromList

addToTuple (x, y) z = (x, y, z)

printList [] = return ()
printList (x:xs) = do
  print x
  printList xs
