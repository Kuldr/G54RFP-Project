module HillClimbingSolver where

import CNFTypes

neighbours :: Solution -> [Solution]
neighbours (S xs) = [flipNthValue (S xs) i | i <- [0..n]]
                        where n = length xs - 1
flipNthValue :: Solution -> Int -> Solution
flipNthValue (S xs) n = S $ take n xs ++ [flipVar (xs!!n)] ++ drop (n + 1) xs
flipVar :: (Int, Bool) -> (Int, Bool)
flipVar (i,b) = (i, not b)
