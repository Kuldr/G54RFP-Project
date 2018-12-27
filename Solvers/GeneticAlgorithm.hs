module Solvers.GeneticAlgorithm where

import Control.Monad
import System.Random
import Solvers.Naive
import Solvers.HillClimbing
import CNF.Types
import CNF.Evaluator

-- geneticAlgorithm = do
    -- Initial Population
    -- Evaluate Population
    -- Loop on termination criteria returning best
        -- Loop on children to gen
        -- Select Parents
        -- Crossover
        -- Mutation -- Small chance of changing
        -- Selection of new children

initialisePopulation :: Int -> Int -> IO [Solution]
initialisePopulation i v = do
    answer <- replicateM i $ randomSolution v
    return $ answer

evaluatePopulation :: Problem -> [Solution] -> [(Int, Solution)]
evaluatePopulation p ss = map (\s -> (evaluateProblem p s, s)) ss

uniformCrossover :: Solution -> Solution -> IO Solution
uniformCrossover (S ms) (S ds) = do
    ans <- uniformHelper ms ds
    return $ S ans
uniformHelper :: [(Int, Bool)] -> [(Int, Bool)] -> IO [(Int, Bool)]
uniformHelper [] [] = do return $ []
uniformHelper (m:ms) (d:ds) = do
    restCrossover <- uniformHelper ms ds
    randomBool <- randomRIO (True, False)
    if randomBool then return $ m:restCrossover
                  else return $ d:restCrossover

bitFlipMutation :: Solution -> Int -> IO Solution
-- Note can potentially flip a bit and flip it back if given enough bits to flip
bitFlipMutation s 0 = do return s
bitFlipMutation (S xs) n = do
    randomIndex <- randomRIO (0, (length xs - 1))
    let mutation = flipNthValue (S xs) randomIndex
    ans <- bitFlipMutation mutation (n-1)
    return $ ans

-- Other Crossover methods
--      Single Point Crossover
--      K Point Crossover
--      2 Point Crossover

-- Other Mutation methods
--      Bit String Mutation (1/l chance of flipping per bit)
--      Swap 2 Bits
