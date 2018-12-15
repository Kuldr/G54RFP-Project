module CNFEvaluator where

import CNFTypes

evaluateProblem :: Problem -> Solution -> Int
evaluateProblem (P cs) s = length $ filter (\c -> evaluateClause c s) cs
evaluateClause :: Clause -> Solution -> Bool
evaluateClause (C ls) s = or [evaluateLiteral l s | l <- ls]
evaluateLiteral :: Literal -> Solution -> Bool
evaluateLiteral (Positive v) s = evaluateVariable v s
evaluateLiteral (Negative v) s = not $ evaluateVariable v s
evaluateVariable :: Var -> Solution -> Bool
evaluateVariable (V x) (S ((i,b):xs)) = if i == x then b
                                        else evaluateVariable (V x) (S xs)
