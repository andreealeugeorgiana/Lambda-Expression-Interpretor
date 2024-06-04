module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (Abs x e) = nub $ x : vars e
vars (App e1 e2) = nub $ vars e1 ++ vars e2

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (Abs x e) = freeVars e \\ [x]
freeVars (App e1 e2) = nub $ freeVars e1 ++ freeVars e2

-- 1.3.
chars :: [String]
chars = map return ['a' .. 'z'] ++ [a : b | b <- "" : chars, a <- ['a' .. 'z']]

newVar :: [String] -> String
newVar xs = head  $ filter (`notElem` xs) chars

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Macro _) = True

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = reduce' e1
  where
    reduce' (Var y)
        | y == x = e2
        | otherwise = Var y
    reduce' (Abs y e)
        | y == x = Abs y e
        | y `elem` freeVars e2 = 
            let z = newVar (vars e1 ++ vars e2)
            in Abs z (reduce' (subst y (Var z) e))
        | otherwise = Abs y (reduce' e)
    reduce' (App e1' e2') = App (reduce' e1') (reduce' e2')

subst :: String -> Lambda -> Lambda -> Lambda
subst x s (Var y)
    | y == x = s
    | otherwise = Var y
subst x s (Abs y e)
    | y == x = Abs y e
    | otherwise = Abs y (subst x s e)
subst x s (App e1 e2) = App (subst x s e1) (subst x s e2)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e) e2) = reduce x e e2
normalStep (App e1 e2) = if (normalStep e1) == e1 then App e1 (normalStep e2) else App (normalStep e1) e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App e1 e2)
    | not (isNormalForm e1) = App (applicativeStep e1) e2
    | not (isNormalForm e2) = App e1 (applicativeStep e2)
    | otherwise = case e1 of
                    Abs x e -> reduce x e e2
                    _ -> App e1 e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep e = e

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step expr = expr : if isNormalForm expr
                            then []
                            else simplify step (step expr)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
