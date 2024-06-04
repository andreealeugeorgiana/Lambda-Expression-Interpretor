module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
bTrue = k
bFalse = ki
bAnd = Abs "x" $ Abs "y" $ App (App vx vy) vx
bOr = Abs "x" $ Abs "y" $ App (App vx vx) vy
bNot = Abs "x" $ App (App vx ki) k
bXor = Abs "x" $ Abs "y" $ App (App vx (App bNot vy)) vy

-- 4.2. Pair encodings
pair = Abs "x" $ Abs "y" $ Abs "z" $ App (App vz vx) vy
first = Abs "z" $ App vz k
second = Abs "z" $ App vz ki

-- 4.3. Natural number encodings
n0 = Abs "f" $ i
n1 = Abs "f" $ Abs "x" $ App vf vx
n2 = Abs "f" $ Abs "x" $ App vf (App vf vx)
nSucc = Abs "x" $ Abs "y" $ Abs "z" $ App vy (App (App vx vy) vz)
nPred = Abs "x" $ Abs "y" $ Abs "z" $ App (App (App vx (Abs "m" $ Abs "n" $ App vn (App vm vy))) (Abs "g" $ vz)) (Abs "g" $ vg)
nAdd = Abs "x" $ Abs "y" $ Abs "z" $ Abs "n" $ App (App vx vz) (App (App vy vz) vn)
nSub = Abs "x" $ Abs "y" $ App (App vy nPred) vx
nMult = Abs "x" $ Abs "y" $ Abs "z" $ App vx (App vy vz)

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
