module CNFGenerator where

import System.Random
import Control.Monad
import CNFTypes

-- Generator
-- v is the number of variables to include in the problem
-- c is the number of clauses to include in the problem
-- r is the number of variables per clause
generateProblem :: Int -> Int -> Int -> IO Problem
generateProblem v c r = do
                            clauses <- replicateM c (generateClause v r)
                            return $ P clauses
generateClause :: Int -> Int -> IO Clause
generateClause v r = do
                        literals <- replicateM r (generateLiteral v)
                        return $ C literals
generateLiteral :: Int -> IO Literal
generateLiteral v = do
                        randomBool <- randomRIO (True, False)
                        variable   <- generateVariable v
                        if randomBool then return $ Positive variable
                                      else return $ Negative variable
generateVariable :: Int -> IO Var
generateVariable v = do
                        randomVar <- randomRIO (1,v+1)
                        return $ V randomVar
