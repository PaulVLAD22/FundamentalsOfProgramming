data Prog = On Instr
data Instr = Off | Expr :> Instr
data Expr = Mem | V Int | Expr :+ Expr
type Env = Int -- valoarea celulei de memorie
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On instr) = []

stmt :: Instr -> DomInstr
stmt Off env = []
stmt (e :> instr) env = expr e env : stmt instr (expr e env)

expr :: Expr -> DomExpr
expr Mem env = env
expr (V i) env = i
expr (t1 :+ t2) env = expr t1 env + expr t2 env


t=On ((V 3) :> ((Mem :+ (V 5)) :> Off))


type Name = String
data Hask = HTrue
    | HFalse
    | HLit Int
    | HIf Hask Hask Hask
    | Hask :==: Hask
    | Hask :+: Hask
    | HVar Name
    | HLam Name Hask
    | Hask :$: Hask
    deriving (Read, Show)

infix 4 :==:
infixl 6 :+:
infixl 9 :$:

data Value = VBool Bool
    | VInt Int
    | VFun (Value -> Value)
    | VError -- pentru reprezentarea erorilor

type HEnv = [(Name, Value)]
type DomHask = HEnv -> Value

instance Show Value where
    show (VInt i) = show i 
    show (VFun f) = "Function"
    show VError = "Error"
    show (VBool b) = show b

instance Eq Value where 
    VInt i == VInt j = i==j
    VBool b1 == VBool b2 = b1 == b2
    VFun f == VFun f2 = error "Functions"
    VError == VError = error "Error"
    _ == _ = error "different types not comparable"

hEval :: Hask -> DomHask

hEval HTrue env = VBool True 
hEval HFalse env = VBool False 
hEval (HLit i) env = VInt i 
hEval (HIf h1 h2 h3) env = case hEval h1 env of 
    VBool True -> hEval h2 env 
    VBool False -> hEval h3 env
hEval (h1 :==: h2) env = VBool (hEval h1 env == hEval h2 env)
hEval (d :+: e) r 	=	hadd (hEval d r) (hEval e r)
	where 	
		hadd (VInt i) (VInt j) = VInt (i + j)
		hadd _ _ = VError
hEval (HVar n) env = case lookup n env of 
    Nothing -> VError
    Just x -> x
hEval (HLam x e ) r = VFun (\v -> hEval e ((x,v):r))





