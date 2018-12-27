import Solvers.HillClimbing
import Solvers.Naive
import CNF.Generator
import CNF.Evaluator
import CNF.Types

-- Note these tests aren't unit tests due to the randomness but they will check
--      that the solvers are working

v, c, r, m :: Int
v = 50
c = 200
r = 3
m = 1000

-- Test a hill climbing solver
--      where f is the relevant setup function
testHC f =
    do
        problem <- generateProblem v c r
        solution <- f problem v m
        putStrLn $ show solution
        putStrLn $ show $ evaluateProblem problem solution

-- Test a Naive Solver
--      where f is the relevant setup function
testNaive f =
    do
        problem <- generateProblem v c r
        let solution = f v
        putStrLn $ show solution
        putStrLn $ show $ evaluateProblem problem solution
testNaiveM f = -- Monadic solver version for random
    do
        problem <- generateProblem v c r
        solution <- f v
        putStrLn $ show solution
        putStrLn $ show $ evaluateProblem problem solution
