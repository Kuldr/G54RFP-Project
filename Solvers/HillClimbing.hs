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
uphillNeighbours p s = filter (\n -> evaluateProblem p n > evaluateProblem p s ) $ neighbours s
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
--      Evaluate all neighbours and then choose the one that makes the most progress
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

-- Simple Hill Climbing
--      Choose the first neighbour that improves
--      Due to evaluating all neighbours this implementation isn't faster than
--          steepest ascent. Benefits can be found with greater exploration though
-- Setup the solver with problem p with v variables and m max steps to take
setUpSimpleHC :: Problem -> Int -> Int -> IO Solution
setUpSimpleHC p v m =
    do
        startingSolution <- randomSolution v -- Starts with random solution
        return $ simpleHC p startingSolution m
simpleHC :: Problem -> Solution -> Int -> Solution
simpleHC p s 0 = s
simpleHC p s m =
    if next == s then s -- returns early if there are no improving neighbours
        else simpleHC p next (m-1)
            where next = head $ (uphillNeighbours p s) ++ [s]

-- Stochastic Hill Climb
--      Randomly choose a neighbour and if they improve select them
-- Setup the solver with problem p with v variables and m max steps to take
--      Can finish early is it can't find an improving solution
setUpStochastic :: Problem -> Int -> Int -> IO Solution
setUpStochastic p v t =
    do
        startingSolution <- randomSolution v -- Starts with random solution
        answer <- stochasticHC p startingSolution t
        return $ answer
stochasticHC :: Problem -> Solution -> Int -> IO Solution
stochasticHC p s 0 = return s
stochasticHC p s m =
    do
        putStrLn $ show m --DEBUG
        nextSolution <- stochasticPickNeighbour p s (neighbours s)
        if nextSolution == s then return s
            else do
                answer <- stochasticHC p nextSolution (m-1)
                return $ answer
-- Randomly test if a neighbour is better than the current solution
--      If all neighbours don't improve return original solution
stochasticPickNeighbour :: Problem -> Solution -> [Solution] -> IO Solution
stochasticPickNeighbour p s [] = do return s
stochasticPickNeighbour p s ns =
    do
        randomIndex <- randomRIO (0, (length ns - 1))
        let n = ns!!randomIndex
        let remainingNeighbours = take randomIndex ns ++ drop (randomIndex+1) ns
        if evaluateProblem p n > evaluateProblem p s
            then return $ n
            else stochasticPickNeighbour p s remainingNeighbours

-- OTHER HILL CLIMBING MAY IMPLEMENT
--Stochastic
--  Select Random neighbour and evaluate
--  Use that if it makes an improvement
-- Different Stochastic Hill Climb where chooses all uphill at random
    -- As this takes as long to run as Steepest Ascent

-- Remember can return early if no better solution
