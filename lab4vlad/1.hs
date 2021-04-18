
type Name = String

data Term = Var Name
          | Con Integer
          | Term :+: Term
          | Lam Name Term
          | App Term Term
  deriving (Show)

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)

newtype Identity a = Identity { runIdentity :: a }

instance  Monad Identity where
  return va = Identity va
  ma >>= k = k $ runIdentity ma
 
instance  Applicative Identity where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor Identity where              
  fmap f ma = pure f <*> ma    

type M = Identity

instance Show a => Show (Identity a) where
    show x = show $ runIdentity x

showM :: Show a => M a -> String
showM =show . runIdentity

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


test :: Term -> String
test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1
