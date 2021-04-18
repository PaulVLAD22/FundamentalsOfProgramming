import Data.Maybe ( fromMaybe )
data Prog = On Instr

data Instr = Off
    | Expr :> Instr

data Expr = Mem
    | V Int
    | Expr :+ Expr

type Env = Int -- valoarea celulei de memorie
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On s) = stmt s 0

stmt :: Instr -> DomInstr
stmt Off _= []
stmt (e :> a) m =expr e m : stmt a (expr e m)



expr :: Expr -> DomExpr
expr (V a) _ = a
expr Mem b = b
expr (e1 :+ e2 ) m = expr e1 m + expr e2 m


-- ce n ai stiut: mereu trebuie sa tii undeva valoarea env si sa gandesti unde ii e locu
-- citeste mai bine cerinta

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
    show (VBool b) = show b
    show (VInt i )= show i
    show (VFun f) = show "Functie"
    show (VError) = show "Error"
    
instance Eq Value where
    VBool a == VBool b = a==b
    VInt i == VInt j = i==j
    _ == _  = error " Nu se pot compara "

hEval :: Hask -> DomHask

hEval HTrue env = VBool True 
hEval HFalse env = VBool False 
hEval (HLit i) env = VInt i
hEval (HIf h1 h2 h3) env = case hEval h1 env of
    VBool True -> hEval(h2) env
    VBool False -> hEval(h3) env
hEval (h1 :==: h2) env = case  hEval h1 env == hEval h2 env of
    True -> VBool True 
    False -> VBool False 
hEval (d :+: e) r 	=	hadd (hEval d r) (hEval e r)
	where 	
		hadd (VInt i) (VInt j) = VInt (i + j)
		hadd _ _ = VError

hEval (HVar x) r = fromMaybe VError (lookup x r)

hEval (HLam x e ) r = VFun (\v -> hEval e ((x,v):r))
