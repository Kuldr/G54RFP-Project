module Solvers.Naive where

import System.Random
import Control.Monad
import CNF.Types

-- Generates a solution w/ v variables that are True or False respecitvely
allTrueSolution :: Int -> Solution
allTrueSolution v = S [(i, True) | i <- [1..v]]
allFalseSolution :: Int -> Solution
allFalseSolution v = S [(i, False) | i <- [1..v]]

-- Generates a solution w/ v variables that are randomly True or False
randomSolution :: Int -> IO Solution
randomSolution v = do
                    bs <- replicateM v (randomRIO (True, False))
                    return $ S $ zip [1..v] bs
