import Data.Maybe
import Data.List

type Name = String

data  Pgm  = Pgm [Name] Stmt
        deriving (Read, Show)

data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt| Name := AExp| While BExp Stmt
        deriving (Read, Show)

data AExp = Lit Integer 
  | AExp :+: AExp 
  | AExp :*: AExp
  | Var Name
  deriving (Read, Show)

data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp
        deriving (Read, Show)

infixr 2 :::
infix 3 :=
infix 4 :==:
infixl 6 :+:
infixl 7 :*:

newtype IntState a = IntState { runIntState :: Integer -> (a, Integer) }

instance Show a => Show (IntState a) where
    show x = show $ runIntState x 0

instance Functor IntState where
     fmap f ma = pure f <*> ma

instance Applicative IntState where
    pure = return
    mf <*> ma = do
        f <- mf
        f <$> ma

instance Monad IntState where
    return a = IntState (\s -> (a, s))
    
    m >>= k = IntState (\s ->
        let (a, aState) = runIntState m s
        in runIntState (k a) aState)




type Env = [(Name, Integer)]

modify :: (Integer -> Integer) -> IntState ()
modify f = IntState (\s -> ((), f s))

get :: IntState Integer
get = IntState (\s -> (s, s))

tickS :: IntState ()
tickS = modify (+ 1)-- la writer dam parametru string care se adauga
-- aici dam parametru functie care modifica starea in functie de cum vrem


add (Lit i) (Lit j) = return (Lit $ i + j)
add _ _ = return $ Lit 0

interp (Var x) env = case lookup x env of 
  Nothing->return $ Lit 0
  Just x -> return $ Lit x

interp (Lit i) _ = return $ Lit i 

interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    tickS
    add v1 v2


aEval :: AExp -> Env -> Integer
aEval (Lit i) env= i 
aEval (Var name) env = case lookup name env of 
  Nothing->error "variable doesn't exist"
  Just x -> x
aEval (a1 :*: a2) env = aEval a1 env * aEval a2 env
aEval (a1 :+: a2 ) env= aEval a1 env + aEval a2 env 

bEval :: BExp -> Env -> Bool
bEval BTrue env = True 
bEval BFalse env = False 
bEval (a1 :==: a2) env = aEval a1 env == aEval a2 env
bEval (Not b) env = not (bEval b env)


sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (st1 ::: st2) env = sEval st2 (env++sEval st1 env)
sEval (If b st1 st2) env =  if (bEval b env) then (sEval st1 env) else (sEval st2 env) 

sEval (name := exp) env =  (name, (aEval exp env)) : filter (\(k,v) -> k /= name) env

sEval (While cond st) env = case (bEval cond env) of
  True -> sEval (While cond st) (sEval st env)
  False -> env


envFromNames :: [Name] -> Env
envFromNames names = [(name, 0) | name <- names]

pEval :: Pgm -> Env
pEval (Pgm lvar st) = sEval st (envFromNames lvar)  


 
factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3 :::
  While (Not (Var "n" :==: Lit 0))
    ( "p" := Var "p" :*: Var "n" :::
      "n" := Var "n" :+: Lit (-1)
    )


test1 test= pEval $ Pgm [] test

