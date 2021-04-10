import Data.Maybe
data Prog = On Instr
data Instr = Off | Expr :> Instr
data Expr = Mem | V Int | Expr :+ Expr
type Env = Int -- valoarea celulei de memorie
type DomProg = [Int]
type DomInstr = Env -> [Int]
type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On s) = stmt s 0

stmt :: Instr -> DomInstr
stmt (e :> s) m = let v = expr e m in (v: (stmt s v))
stmt Off _ = []

expr :: Expr -> DomExpr
expr (e1 :+ e2) m = (expr e1 m) + (expr e2 m)
expr Mem m = m
expr (V a) _ = a

p1 = On ( (V 3) :> ((Mem :+ (V 5)):> Off))
p2 = On ( (V 5) :> ((Mem :+ (V 4)):> (((Mem :+ (V 3))) :> Off)))





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
	show (VBool val) = show val
	show (VInt val) = show val
	show (VFun _ ) = "functie"
	show (VError) = "eroare"
	
instance Eq Value where
	(VBool b1) == (VBool b2) = b1 == b2
	(VInt i1) == (VInt i2) = i1 == i2
	(VFun _) == (VFun _) = error "Nu se pot compara 2 functii"
	(VError) == (VError) = error "Nu se pot compara 2 erori"
	_ == _ = error "Nu se pot compara"

hEval :: Hask -> DomHask
hEval HTrue r = VBool True
hEval HFalse r = VBool False
hEval (HLit i) r = VInt i

hEval (HIf c d e) r = 
	if hEval c r == VBool True then hEval d r else hEval e r
	
hEval (d :==: e) r 	=	VBool (hEval d r == hEval e r)

hEval (d :+: e) r 	=	hadd (hEval d r) (hEval e r)
	where 	
		hadd (VInt i) (VInt j) = VInt (i + j)
		hadd _ _ = VError

hEval (HVar x) r = fromMaybe VError (lookup x r)

hEval (HLam x e ) r = VFun (\v -> hEval e ((x,v):r))
