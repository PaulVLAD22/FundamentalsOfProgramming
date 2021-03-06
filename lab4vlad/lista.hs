type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Fail 
          | Amb Term Term
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

add (Num x) (Num y) = return $ Num (x+y) 
add _ _ =return Wrong 

interp :: Term -> Environment -> M Value
interp (Amb t1 t2) env = interp t1 env ++ interp t2 env
interp (Fail) env = []
interp (App t1 t2) env = do 
                      x1<-interp t1 env 
                      x2<- interp t2 env 
                      apply x1 x2
interp (Lam x e) env = return $ Fun (\v -> interp e ((x,v):env))
interp (Con x) env = return $ Num x 

interp (Var nume) env = let v = lookup nume env in case v of
                            Nothing -> return Wrong
                            Just i -> return i
interp (t1 :+: t2) env = do 
                        x1 <- interp t1 env
                        x2 <-interp t2 env  
                        add x1 x2

test :: Term -> String
test t = show $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1
