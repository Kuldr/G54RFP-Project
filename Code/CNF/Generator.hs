module CNF.Generator where

import System.Random
import Control.Monad
import CNF.Types

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
                        variable   <- generateVariable v
                        randomBool <- randomRIO (True, False)
                        if randomBool then return $ Positive variable
                                      else return $ Negative variable
generateVariable :: Int -> IO Var
generateVariable v = do
                        randomVar <- randomRIO (1,v)
                        return $ V randomVar
