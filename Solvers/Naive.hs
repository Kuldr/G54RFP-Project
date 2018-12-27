module Solvers.Naive where

import System.Random
import Control.Monad
import CNF.Types

allTrueSolution :: Int -> Solution
allTrueSolution v = S [(i, True) | i <- [1..v]]

allFalseSolution :: Int -> Solution
allFalseSolution v = S [(i, False) | i <- [1..v]]

randomSolution :: Int -> IO Solution
randomSolution v = do
                    bs <- replicateM v (randomRIO (True, False))
                    return $ S $ zip [1..v] bs
