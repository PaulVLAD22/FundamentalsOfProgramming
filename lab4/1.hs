newtype Identity a = Identity { runIdentity :: a }

-- identity
instance Monad Identity where
    return va = Identity va
    ma >>= k = k $ runIdentity ma

-- functor si applicative sunt mereu la fel
instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

instance Show a => Show (Identity a) where
    show x = show $ runIdentity x

instance Applicative Identity where
    pure = return
    mf <*> ma = do
        f <- mf
        f <$> ma

type M = Identity

--lab 4 inceput

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

type Environment = [(Name, Value)]

--
-- test asta n am inteles
test :: Term -> String
test t = showM $ interp t []
--

showM :: Show a => M a -> String
showM = show . runIdentity

lookupM :: Name -> Environment -> M Value
lookupM x env = case lookup x env of
    Just v -> return v
    Nothing -> return Wrong

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j) --adunam numai numere
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) v = k v -- aplica functie peste valoare (cica k pune v in monada)
apply _ _ = return Wrong

--

interp :: Term -> Environment -> M Value
interp (Var x) env = lookupM x env
interp (Con i) _ = return $ Num i
interp (Lam x e) env = return $
    Fun $ \v -> interp e ((x, v) : env)--evaluam functia adaugand la mediul de evaluare valoarea v (parametru)

interp (t1 :+: t2) env = do
    v1 <- interp t1 env -- interpretam t1 in functie de env
    v2 <- interp t2 env
    add v1 v2

interp (App t1 t2) env = do
    f <- interp t1 env
    v <- interp t2 env
    apply f v

pgm :: Term
pgm = App
        (Lam "x" (Var "x" :+: Var "x"))
        (Con 10 :+: Con 11)

myTest = do
    test pgm