type Name = String
data Pgm = Pgm [Name] Stmt
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

envFromNames :: [Name] -> Env
envFromNames names = [(name, 0) | name <- names]

pEval :: Pgm -> Env
pEval (Pgm l st) = sEval st (envFromNames l)

sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (s1 ::: s2) env = sEval s2 (sEval s1 env)
sEval (If b i1 i2) env = case bEval b env of 
    True -> sEval i1 env
    False -> sEval i2 env

sEval (While cond st) env = case (bEval cond env) of
    True -> sEval (While cond st) (sEval st env)
    False -> env
sEval (name := exp) env = (name, (aEval exp env)) : filter (\(k,v) -> k /= name) env


bEval :: BExp -> Env -> Bool
bEval BTrue env = True 
bEval BFalse env = False 
bEval (a1 :==: a2) env = aEval a1 env == aEval a2 env
bEval (Not b) env = Not b

aEval :: AExp -> Env -> Integer
aEval (Lit i) env = i
aEval (a1 :+: a2) env = aEval a1 env + aEval a2 env
aEval (a1 :*: a2) env = aEval a1 env * aEval a2 env
aEval (Var name) env = case lookup name env of 
  Nothing -> Nothing 
  Just x -> x 

