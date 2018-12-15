import System.Random
import Control.Monad

-- Types
--     A Boolean Problem in CNF is a series of clauses which are AND'd together
--     Each of these clauses are a series of variables (literals) OR'd together
--     Each of the literals could be negatated (NOT) denoated by +ive/-ive
--     The literals use intergers to track the variables
--     Based upon the formulation of the problem at http://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
newtype Problem = P [Clause] deriving (Show)
newtype Clause  = C [Literal] deriving (Show)
data    Literal = Positive Var | Negative Var deriving (Show)
newtype Var     = V Int deriving (Show)

-- Generator
-- v is the number of variables to include in the problem
-- c is the number of clauses to include in the problem
-- r is the number of variables per clause
generateProblem :: Int -> Int -> Int -> IO Problem
generateProblem v c r = do
                            clauses <- replicateM c (generateClause v r)
                            return (P clauses)

generateClause :: Int -> Int -> IO Clause
generateClause v r = do
                        literals <- replicateM r (generateLiteral v)
                        return (C literals)

generateLiteral :: Int -> IO Literal
generateLiteral v = do
                        randomBool <- randomRIO (True, False)
                        variable   <- generateVariable v
                        if randomBool then return (Positive variable)
                                      else return (Negative variable)

generateVariable :: Int -> IO Var
generateVariable v = do
                        randomVar <- randomRIO (1,v+1)
                        return (V randomVar)
