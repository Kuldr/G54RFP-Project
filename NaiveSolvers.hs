import CNFTypes

allTrueSolution :: Int -> Solution
allTrueSolution v = S [(i, True) | i <- [1..v]]

allFalseSolution :: Int -> Solution
allFalseSolution v = S [(i, False) | i <- [1..v]]
