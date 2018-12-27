module CNF.Evaluator where

import CNF.Types

-- Evaluator
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

-- Other ways of evaluating the problem
-- evaluateProblem (P cs) s = sum [if evaluateClause c s then 1 else 0 | c <- cs]
-- evaluateProblem (P cs) s = foldl (evaluateClauses s) 0 cs
-- evaluateClauses :: Solution -> Int -> Clause -> Int
-- evaluateClauses s i c = if evaluateClause c s then i+1 else i
