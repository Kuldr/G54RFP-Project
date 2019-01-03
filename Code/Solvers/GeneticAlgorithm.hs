module Solvers.GeneticAlgorithm where

import Data.List
import Data.Ord
import Control.Monad
import System.Random
import Solvers.Naive
import Solvers.Common
import CNF.Types
import CNF.Evaluator

geneticAlgorithm :: Int -> Int -> Problem -> Int -> IO Solution
geneticAlgorithm i v p g = do
    -- Initialise Population
    pop <- initialisePopulation i v
    -- Evaluate Population
    let evalPop = evaluatePopulation p pop
    -- Loop for g generations returning best
    best <- gaLoop evalPop g p
    return $ best
gaLoop :: [(Int, Solution)] -> Int -> Problem -> IO Solution
gaLoop ps 0 _ = do return $ best ps
gaLoop ps g p = do
    -- This method creates 1/4 pop as children
    cs <- replicateM ((length ps) `div` 4) (createChild ps p)
    -- Replacement
    let newps = weakestReplacements ps cs
    ans <- gaLoop newps (g-1) p
    return ans
createChild :: [(Int, Solution)] -> Problem -> IO (Int, Solution)
createChild ps p = do
    -- Select Parents, could select same parents
    p1 <- stochasticSelection ps
    p2 <- stochasticSelection ps
    -- Crossover
    c <- uniformCrossover p1 p2
    -- Mutation
    randomChance <- randomRIO (0.0,1.0) :: IO Double
    if randomChance < 0.1 -- 10% chance
        then do
                m <- bitFlipMutation c 1
                return $ (evaluateProblem p m, m)
        else return $ (evaluateProblem p c, c)

initialisePopulation :: Int -> Int -> IO [Solution]
initialisePopulation i v = do
    answer <- replicateM i $ randomSolution v
    return $ answer

evaluatePopulation :: Problem -> [Solution] -> [(Int, Solution)]
evaluatePopulation p ss = map (\s -> (evaluateProblem p s, s)) ss

stochasticSelection :: [(Int, Solution)] -> IO Solution
stochasticSelection is = do
    randomIndex <- randomRIO (0, (length is - 1))
    return $ snd $ is!!randomIndex

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

weakestReplacements :: [(Int, Solution)] -> [(Int, Solution)] -> [(Int, Solution)]
weakestReplacements ps [] = ps
weakestReplacements ps (c:cs) = weakestReplacements newps cs
                                    where newps = weakestReplacement ps c
weakestReplacement :: [(Int, Solution)] -> (Int, Solution) -> [(Int, Solution)]
weakestReplacement ps c = c:(drop 1 $ sortBy (comparing fst) ps)

-- Other Selection methods
--      Tournament Selection
--      Roulette Wheel Selection
--      Truncation Selection

-- Other Crossover methods
--      Single Point Crossover
--      K Point Crossover
--      2 Point Crossover

-- Other Mutation methods
--      Bit String Mutation (1/l chance of flipping per bit)
--      Swap 2 Bits

-- Other Replacement methods
--      Take new children as the new population (requires the same amount of children)
--      Take n fittest out of parents and children
--      Random Replacement
