module Solver.HillClimbing where

import Data.List
import Data.Ord
import System.Random
import CNF.Types
import CNF.Evaluator
import Solvers.Naive


-- Shared Helper Functions
neighbours :: Solution -> [Solution] -- Neighbours includes original solution
neighbours (S xs) = (S xs):[flipNthValue (S xs) i | i <- [0..n]]
                        where n = length xs - 1
flipNthValue :: Solution -> Int -> Solution
flipNthValue (S xs) n = S $ take n xs ++ [flipVar (xs!!n)] ++ drop (n + 1) xs
flipVar :: (Int, Bool) -> (Int, Bool)
flipVar (i,b) = (i, not b)
uphillNeighbours :: Problem -> Solution -> [Solution]
uphillNeighbours p (S xs) = [s | i <- [0..n],
                                        let s = flipNthValue (S xs) i,
                                        evaluateProblem p s > e ]
                        where n = length xs - 1
                              e = evaluateProblem p (S xs)

-- Steepest Ascent
--  Evaluate all and then choose best
-- Set up the steepest ascent hill climb
--      This is the function to call at the begining
setUpSAHC :: Problem -> Int -> Int -> IO Solution
setUpSAHC p v t =
    do
        startingSolution <- randomSolution v -- Starts with random solution
        return $ steepestAscentHillClimb p startingSolution t
-- Main Loop
steepestAscentHillClimb :: Problem -> Solution -> Int -> Solution
steepestAscentHillClimb p s 0 = s
steepestAscentHillClimb p s t =
    if next == s then s
        else steepestAscentHillClimb p next (t-1)
            where next = steepestAscentHillClimbStep p s
-- Single step of hill climb
steepestAscentHillClimbStep :: Problem -> Solution -> Solution
steepestAscentHillClimbStep p s = best $ evalutateAllNeighbours p s
evalutateAllNeighbours :: Problem -> Solution -> [(Int, Solution)]
evalutateAllNeighbours p s = [(evaluateProblem p n, n) | n <- neighbours s]
best :: [(Int, Solution)] -> Solution
best xs = snd $ (sortBy (flip $ comparing fst) xs) !! 0


-- OTHER HILL CLIMBING MAY IMPLEMENT
--Simple Hill Climbing
--  Choose the first neighbour that improves
--Stochastic
--  Select Random neighbour and evaluate
--  Use that if it makes an improvement
-- Different Stochastic Hill Climb where chooses all uphill at random
    -- As this takes as long to run as Steepest Ascent

-- Remember can return early if no better solution
