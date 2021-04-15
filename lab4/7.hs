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

type Name = String
type Environment = [(Name, Value)]

type M a = IntState a

--

data Term = Var Name
            | Con Integer
            | Term :+: Term
            | Lam Name Term
            | App Term Term
            | Count

data Value = Num Integer
            | Fun (Value -> M Value)
            | Wrong

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"
    show Wrong = "<wrong>"

--

modify :: (Integer -> Integer) -> IntState ()
modify f = IntState (\s -> ((), f s))

get :: IntState Integer
get = IntState (\s -> (s, s))

tickS :: IntState ()
tickS = modify (+ 1)

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j)
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = return Wrong

lookupM :: Name -> Environment -> M Value
lookupM x env = case lookup x env of
    Just v -> return v
    Nothing -> return Wrong

--

interp :: Term -> Environment -> M Value
interp (Var x) env = lookupM x env
interp (Con i) _ = return $ Num i
interp (Lam x e) env = do
    return $ Fun $ \v -> interp e ((x, v) : env)

interp Count _ = return Wrong

interp (Count :+: t1) env = do
    v1 <- interp t1 env
    tickS
    return v1

interp (t1 :+: Count) env = do
    v1 <- interp t1 env
    tickS
    return v1

interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    tickS
    add v1 v2

interp (App t1 t2) env = do
    f <- interp t1 env
    v <- interp t2 env
    tickS
    apply f v

pgm :: Term
pgm = App
        (Lam "x" (Var "x" :+: Var "x"))
        (Con 10 :+: Con 20)