
type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
          | Out Term
  deriving (Show)


newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }


instance  Monad StringWriter where
  return va = StringWriter (va, "")
  ma >>= k = let (va, log1) = runStringWriter ma
                 (vb, log2) = runStringWriter (k va)
             in  StringWriter (vb, log1 ++ log2)


instance  Applicative StringWriter where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor StringWriter where              
  fmap f ma = pure f <*> ma     

tell :: String -> StringWriter () 
tell log = StringWriter ((), log)

type M = StringWriter

instance Show a => Show (StringWriter a) where
    show ma = let (a,s) = runStringWriter ma 
            in
            s



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
apply _ _ = return Wrong 


interp :: Term -> Environment -> M Value
interp (Var n) env =  case lookup n env of
    Just v -> return v
    Nothing -> return Wrong
interp (Con i) env = return $ Num i

interp (m1 :+: m2) env= do
    t1<-interp (m1) env
    t2<-interp (m2) env
    case (t1,t2) of
        (Num i,Num j) -> return $ Num (i+j)
        (_,_) -> return Wrong

interp (Lam x e) env = return $ Fun (\v -> interp e ((x,v):env))
interp (App t1 t2) env = do
    f <- interp t1 env
    v <- interp t2 env
    apply f v

interp (Out t) env = do
    a <- interp t env
    tell (show a++";")
    return a 




test :: Term -> String
test t = show $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1
