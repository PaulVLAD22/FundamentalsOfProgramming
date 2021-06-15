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


envFromNames names = [(name,0)| name<-names]



pEval :: Pgm -> Env
pEval (Pgm names st) = sEval st (envFromNames names)


sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (st1:::st2) env = sEval st2 (sEval st1 env)
sEval (If bexp st1 st2) env = if (bEval bexp env) then sEval st1 env else sEval st2 env
sEval (While bexp st1) env = if (bEval bexp env) then sEval (st1:::st1) env else env
sEval (name := aexp) env = (name, (aEval aexp env)) : filter (\(k,v) -> k /= name) env

bEval :: BExp -> Env -> Bool
bEval BTrue env = True
bEval BFalse env = False 
bEval (aexp1 :==: aexp2) env  = if (aEval aexp1 env == aEval aexp2 env) then True else False 
bEval (Not bexp) env = not $ bEval bexp env 


aEval :: AExp -> Env -> Integer
aEval (Lit x) env  = x 
aEval (a1 :+: a2) env = aEval a1 env + aEval a2 env 
aEval (a1 :*: a2 ) env = aEval a1 env * aEval a2 env 
aEval (Var nume) env = let v = lookup nume env in case v of
                            Nothing -> error "Variabila inexistenta"
                            Just i -> i


factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3:::
  While (Not (Var "n" :==: Lit 0))
  ( "p" := Var "p" :*: Var "n" :::
  "n" := Var "n" :+: Lit (-1)
  )
pg1 = Pgm [] factStmt