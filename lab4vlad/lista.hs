
type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Fail 
          | Amb Term Term
          | Lst [Term]
  deriving (Show)

type M a= [a]

showM :: Show a => M a -> String
showM x = show x

data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]

apply :: Value -> Value -> M Value
apply (Fun k) v = k v -- aplica functie peste valoare (cica k pune v in monada)
apply _ _ = []


interp :: Term -> Environment -> M Value
interp (Var n) env =  case lookup n env of
    Just v -> return v
    Nothing -> []
interp (Con i) env = return $ Num i

interp (m1 :+: m2) env= do
    t1<-interp (m1) env
    t2<-interp (m2) env
    case (t1,t2) of
        (Num i,Num j) -> return $ Num (i+j)
        (_,_) -> []

interp (Lam x e) env = return $ Fun (\v -> interp e ((x,v):env))
interp (App t1 t2) env = do
    f <- interp t1 env
    v <- interp t2 env
    apply f v
interp Fail env = []
interp (Amb t1 t2) env = do 
    t3 <- interp t1 env  
    t4 <-interp t2 env 
    [t3,t4]

test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1
