module Solvers.HillClimbing where

import Data.List
import Data.Ord
import System.Random
import CNF.Types
import CNF.Evaluator
import Solvers.Naive


-- Shared Helper Functions
-- Generating list of neighbouring solutions
neighbours :: Solution -> [Solution]
neighbours (S xs) = [flipNthValue (S xs) i | i <- [0..n]]
                        where n = length xs - 1
uphillNeighbours :: Problem -> Solution -> [Solution]
uphillNeighbours p (S xs) = [s | i <- [0..n],
                                        let s = flipNthValue (S xs) i,
                                        evaluateProblem p s > e ]
                        where n = length xs - 1
                              e = evaluateProblem p (S xs)
flipNthValue :: Solution -> Int -> Solution
flipNthValue (S xs) n = S $ take n xs ++ [flipVar (xs!!n)] ++ drop (n + 1) xs
flipVar :: (Int, Bool) -> (Int, Bool)
flipVar (i,b) = (i, not b)
-- Evaluating neighbouring solutions
evalutateAllNeighbours :: Problem -> Solution -> [(Int, Solution)]
evalutateAllNeighbours p s = [(evaluateProblem p n, n) | n <- neighbours s]
best :: [(Int, Solution)] -> Solution
best xs = snd $ (sortBy (flip $ comparing fst) xs) !! 0

-- Steepest Ascent Hill Climb
--  Evaluate all neighbours and then choose the one that makes the most progress
-- Setup the solver with problem p with v variables and m max steps to take
--      Can finish early is it can't find an improving solution
setUpSteepestAscent :: Problem -> Int -> Int -> IO Solution
setUpSteepestAscent p v m =
    do
        startingSolution <- randomSolution v -- Starts with random solution
        return $ steepestAscent p startingSolution m
-- Main Loop
steepestAscent :: Problem -> Solution -> Int -> Solution
steepestAscent p s 0 = s
steepestAscent p s m =
    if next == s then s -- returns early if there are no improving neighbours
        else steepestAscent p next (m-1)
            where next = best $ (evaluateProblem p s, s):(evalutateAllNeighbours p s)

-- OTHER HILL CLIMBING MAY IMPLEMENT
--Simple Hill Climbing
--  Choose the first neighbour that improves
--Stochastic
--  Select Random neighbour and evaluate
--  Use that if it makes an improvement
-- Different Stochastic Hill Climb where chooses all uphill at random
    -- As this takes as long to run as Steepest Ascent

-- Remember can return early if no better solution
