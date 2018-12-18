module HillClimbingSolvers where

import CNFTypes

-- Shared Helper Functions
neighbours :: Solution -> [Solution] -- Neighbours includes original solution
neighbours (S xs) = (S xs):[flipNthValue (S xs) i | i <- [0..n]]
                        where n = length xs - 1
flipNthValue :: Solution -> Int -> Solution
flipNthValue (S xs) n = S $ take n xs ++ [flipVar (xs!!n)] ++ drop (n + 1) xs
flipVar :: (Int, Bool) -> (Int, Bool)
flipVar (i,b) = (i, not b)

-- OTHER HILL CLIMBING MAY IMPLEMENT
--Simple Hill Climbing
--  Choose the first neighbour that improves
--Stochastic
--  Select Random neighbour and evaluate
--  Use that if it makes an improvement

-- Remember can return early if no better solution
