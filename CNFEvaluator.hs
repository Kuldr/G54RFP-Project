module CNFEvaluator where

import CNFTypes

evaluateProblem :: Problem -> Solution -> Int
evaluateClause :: Clause -> Solution -> Bool
evaluateLiteral :: Literal -> Solution -> Bool
evaluateVariable :: Var -> Solution -> Bool
