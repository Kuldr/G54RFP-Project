module Solvers.Common where

import CNF.Types
import Data.Ord
import Data.List

best :: [(Int, Solution)] -> Solution
best xs = snd $ (sortBy (flip $ comparing fst) xs) !! 0

flipNthValue :: Solution -> Int -> Solution
flipNthValue (S xs) n = S $ take n xs ++ [flipVar (xs!!n)] ++ drop (n + 1) xs
flipVar :: (Int, Bool) -> (Int, Bool)
flipVar (i,b) = (i, not b)
