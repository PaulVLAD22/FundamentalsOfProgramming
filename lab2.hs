import Data.Maybe
import Data.List

type Name = String

data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt | While BExp Stmt | Name := AExp
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


factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )
    
	
	
pg1 = Pgm [] factStmt 


aEval :: AExp -> Env -> Integer
aEval (Lit n) env = n
aEval (e1 :+: e2) env = (aEval e1 env) + (aEval e2 env)
aEval (e1 :*: e2) env = (aEval e1 env) * (aEval e2 env)
-- aEval (Var n) env = fromMaybe (error "Variabila inexistenta") lookup n env - nu prea merge
aEval (Var n) env = let v = lookup n env in case v of
                            Nothing -> error "Variabila inexistenta"
                            Just i -> i

bEval :: BExp -> Env -> Bool
bEval BTrue env = True
bEval BFalse env = False
bEval (e1 :==: e2) env = ((aEval e1 env) == (aEval e2 env))
bEval (Not b) env = not (bEval b env)

--- hippity hoppity your code is now my property
sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (s1 ::: s2) env = sEval s2 (sEval s1 env)
sEval (If cond thenBranch elseBranch) env = case (bEval cond env) of
    True -> sEval thenBranch env
    False -> sEval elseBranch env
sEval (While cond st) env = case (bEval cond env) of
    True -> sEval (While cond st) (sEval st env)
    False -> env
sEval (name := exp) env = (name, (aEval exp env)) : filter (\(k,v) -> k /= name) env

pEval :: Pgm -> Env
pEval (Pgm vars st) = sEval st (envFromNames vars)

envFromNames :: [Name] -> Env
envFromNames names = [(name, 0) | name <- names]
-- 
