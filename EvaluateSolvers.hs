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
