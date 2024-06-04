module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
  substitutedExpr <- getMacros ctx expr
  return $ simplify step substitutedExpr
  where
    getMacros :: Context -> Lambda -> Either String Lambda
    getMacros ctx (Var x) = Right $ Var x
    getMacros ctx (App e1 e2) = do
      e1' <- getMacros ctx e1
      e2' <- getMacros ctx e2
      return $ App e1' e2'
    getMacros ctx (Abs x e) = do
      e' <- getMacros ctx e
      return $ Abs x e'
    getMacros ctx (Macro m) = 
      case lookup m ctx of
        Just expr -> Right expr
        Nothing -> Left $ "error"

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
