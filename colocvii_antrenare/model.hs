{-
Gasiti mai jos  un minilimbaj. Interpretarea este partial definita.
Un program este o expresie de tip Pgmiar rezultatul executiei este ultima stare a memoriei. 
Executia unui program se face apeland pEval.
-}

import Data.Maybe
import Data.List

type Name = String

data  Pgm  = Pgm [(Name,Integer)] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt| Name := AExp | While BExp Stmt
        deriving (Read, Show)

data AExp = Lit Integer | AExp :+: AExp | AExp :*: AExp | Var Name
        deriving (Read, Show)

data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:


type Env = [(Name, Integer)]


aEval :: AExp -> Env -> Integer
aEval (Lit x) env = x 
aEval (Var name) env = let v = lookup name env in case v of
                        Nothing -> error "Variabila inexistenta"
                        Just i -> i
aEval (x:+:y) env = aEval x env +aEval y env
aEval (x:*:y) env = aEval x env *aEval y env


bEval :: BExp -> Env -> Bool
bEval BTrue env = True 
bEval BFalse env = False
bEval (e1 :==: e2) env = aEval e1 env == aEval e2 env
bEval (Not b) env = not $ bEval b env 


sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (st1 ::: st2) env = sEval st2 (sEval st1 env)
sEval (If b st1 st2) env =  if (bEval b env) then (sEval st1 env) else (sEval st2 env) 
sEval (name := exp) env =  (name, (aEval exp env)) : filter (\(k,v) -> k /= name) env
sEval (While logic st) env = if (bEval logic env) then (sEval (While logic st) (sEval st env)) else env




pEval :: Pgm -> (Env, Env)
pEval (Pgm lvar st) = (lvar,sEval st lvar)


envFromNames lvar = [(var,0)| var<-lvar]



 
factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )

test1 = Pgm [("p",1),("n",3)] factStmt 

{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10pct) Adaugati instructiunea While BExp Stmt si interpretarea ei.
3) (20pct) Definiti interpretarea limbajului astfel incat programele sa se execute dintr-o stare 
initiala data iar  pEval  sa afiseze starea initiala si starea finala.

Definiti teste pentru verificarea solutiilor si indicati raspunsurile primite. 

-}