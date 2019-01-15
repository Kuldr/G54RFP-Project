--2.2.1
newtype Problem = P [Clause]
newtype Clause  = C [Literal]
data    Literal = Positive Var | Negative Var
newtype Var     = V Int

--2.3
newtype Solution = S [(Int, Bool)] deriving (Show, Eq)
evaluateProblem :: Problem -> Solution -> Int

--2.4.1
allTrueSolution, allFalseSolution :: Int -> Solution
allTrueSolution  v = S [(i, True)  | i <- [1..v]]
allFalseSolution v = S [(i, False) | i <- [1..v]]

--2.4.2
randomSolution :: Int -> IO Solution
randomSolution v = do bs <- replicateM v (randomRIO (True, False))
                      return $ S $ zip [1..v] bs
