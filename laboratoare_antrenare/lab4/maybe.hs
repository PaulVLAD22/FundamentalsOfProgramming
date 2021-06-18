
type Name = String

data Term = Var Name
  | Con Integer
  | Term :+: Term
  | Lam Name Term
  | App Term Term
  deriving (Show)

data Value = Num Integer
  | Fun (Value -> M Value)
  | Wrong

instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"
  show Wrong = "<wrong>"

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
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

instance Show a => Show (Identity a) where
    show x = show $ runIdentity x

type Environment = [(Name, Value)]
--maybe
type M = Maybe

apply :: Value -> Value -> M Value
apply (Fun k) v = k v -- aplica functie peste valoare (cica k pune v in monada)
apply _ _ = Nothing 

add (Num x) (Num y) = return $ Num (x+y) 
add _ _ = Nothing 

interp :: Term -> Environment -> M Value
interp (App t1 t2) env = do 
                      x1<-interp t1 env 
                      x2<- interp t2 env 
                      apply x1 x2
interp (Lam x e) env = return $ Fun (\v -> interp e ((x,v):env))
interp (Con x) env = return $ Num x 

interp (Var nume) env = let v = lookup nume env in case v of
                            Nothing -> Nothing 
                            Just i -> return i
interp (t1 :+: t2) env = do 
                        x1 <- interp t1 env
                        x2 <-interp t2 env  
                        add x1 x2

-- either