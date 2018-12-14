-- Types
--     A Boolean Problem in CNF is a series of clauses which are AND'd together
--     Each of these clauses are a series of variables (literals) OR'd together
--     Each of the literals could be negatated (NOT) denoated by +ive/-ive
--     The literals use intergers to track the variables
--     Based upon the formulation of the problem at http://people.sc.fsu.edu/~jburkardt/data/cnf/cnf.html
newtype Problem = P [Clause]
newtype Clause  = C [Literal]
data    Literal = Positive Var | Negative Var
newtype Var     = V Int

-- Generator
-- v is the number of variables to include in the problem
-- c is the number of clauses to include in the problem
-- r is the number of variables per clause
generateProblem :: Int -> Int -> Int -> Problem
generateProblem v c r = P [C [Positive (V 1)]]

generateClause :: Int -> Int -> Clause
generateClause v r = C [Positive (V 1)]

generateLiteral :: Int -> Literal
generateLiteral v = Positive (V 1)

generateVariable :: Int -> Var
generateVariable v = V 1
