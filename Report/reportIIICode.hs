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

--2.5.3
neighbours :: Solution -> [Solution]
neighbours s@(S xs) = [flipNthValue s i | i <- [0..n]]
                        where n = length xs - 1

--2.6.1
geneticAlgorithm :: Int -> Int -> Problem -> Int -> IO Solution
geneticAlgorithm i v p g = do
    -- Initialise Population
    -- Evaluate Population
    -- Loop for g generations returning best
gaLoop :: [(Int, Solution)] -> Int -> Problem -> IO Solution
gaLoop ps 0 _ = do return $ best ps
gaLoop ps g p = do
    -- Generate children
    -- Replacement (Merging the old population and the children)
    -- Run loop with new population
createChild :: [(Int, Solution)] -> Problem -> IO (Int, Solution)
createChild ps p = do
    -- Select Parents
    -- Crossover between parents
    -- Mutation of new child
    -- return child
