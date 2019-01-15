--2.2.1
newtype Problem = P [Clause]
newtype Clause  = C [Literal]
data    Literal = Positive Var | Negative Var
newtype Var     = V Int

newtype Solution = S [(Int, Bool)]
