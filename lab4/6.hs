
newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }

instance Show a => Show (StringWriter a) where
    show ma = "Output: " ++ w ++ " Value: " ++ show a
        where (a, w) = runStringWriter ma

instance Functor StringWriter where
    fmap f ma = f <$> ma

instance Applicative StringWriter where
    pure = return
    mf <*> ma = do
        f <- mf
        f <$> ma

instance Monad StringWriter where
    return a = StringWriter (a, mempty)
    m >>= k =   let (v1, log1) = runStringWriter m
                    (v2, log2) = runStringWriter $ k v1
                in StringWriter (v2, log1 ++ log2)

type Name = String
type Environment = [(Name, Value)]

type M a = StringWriter a

-- 

data Term = Var Name
            | Con Integer
            | Term :+: Term
            | Lam Name Term
            | App Term Term
            | Out Term

data Value = Num Integer
            | Fun (Value -> M Value)
            | Wrong

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"
    show Wrong = "<wrong>"

--

tell :: String -> StringWriter ()
tell x = StringWriter((), x)

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
interp (Lam x e) env = return $
    Fun $ \v -> interp e ((x, v) : env)

interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2

interp (App t1 t2) env = do
    f <- interp t1 env
    v <- interp t2 env
    apply f v

interp (Out t) env = do
    v <- interp t env
    tell $ show v ++ "; "
    return v

pgm :: Term
pgm = Out(App
        (Out(Lam "x" (Out(Var "x") :+: Out(Var "x"))))
        (Out(Con 10) :+: Out(Con 11)))

myTest = do
    interp pgm []