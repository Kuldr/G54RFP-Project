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
