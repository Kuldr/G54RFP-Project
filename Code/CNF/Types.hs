module CNF.Types where

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

newtype Solution = S [(Int, Bool)] deriving (Show, Eq)
