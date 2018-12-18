module EvaluateSolvers where

import CNFGenerator
import CNFEvaluator
import NaiveSolvers

generateAndEvaluate :: Int -> Int -> Int -> IO (Int, Int, Int)
generateAndEvaluate v c r =
    do
        -- Generate the problem
        problem <- generateProblem v c r

        -- Solve with Naive Solvers
        let allTrue = allTrueSolution v
        let allFalse = allFalseSolution v
        random <- randomSolution v

        -- Evaluate the solutions
        let allTrueResult = evaluateProblem problem allTrue
        let allFalseResult = evaluateProblem problem allFalse
        let randomResult = evaluateProblem problem random

        -- Return results
        return (allTrueResult, allFalseResult, randomResult)

solveMultiple :: Int -> Int -> Int -> Int -> IO [(Int, Int, Int)]
solveMultiple v c r 0 = do return []
solveMultiple v c r t =
    do
        result <- generateAndEvaluate v c r
        results <- solveMultiple v c r (t-1)
        return $ result : results

averageResult :: [(Int, Int, Int)] -> (Double, Double, Double)
averageResult rs = (trueTotal/n, falseTotal/n, randomTotal/n)
                    where
                        trueTotal   = fromIntegral $ sum [(\(x,_,_) -> x) $ x | x <- rs]
                        falseTotal  = fromIntegral $ sum [(\(_,x,_) -> x) x | x <- rs]
                        randomTotal = fromIntegral $ sum [(\(_,_,x) -> x) x | x <- rs]
                        n = fromIntegral $ length rs
