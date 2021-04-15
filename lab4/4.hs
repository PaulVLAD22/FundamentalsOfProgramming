type M a = [a]

--

type Name = String
data Term = Var Name
            | Con Integer
            | Term :+: Term
            | Lam Name Term
            | App Term Term
            | Amb Term Term
            | Fail

    deriving (Show)

data Value = Num Integer
            | Fun (Value -> M Value)
            | Wrong

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"
    show Wrong = "<wrong>"

type Environment = [(Name, Value)]

--inceput lab6 la moanga

test :: Term -> String
test t = showM $ interp t []

showM :: Show a => M a -> String
showM = show

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j)
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = return Wrong

lookupM :: Name -> Environment -> M Value
lookupM x env = case [v | (y,v) <-env, y==x] of -- returnam lista cu toate valorile cu cheia x
    (v:_) -> return v -- returnam doar prima valoare cu cheia x
    _ -> return Wrong

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
    
-- exclusive listei
interp Fail _ = []
interp (Amb t1 t2) env = interp t1 env ++ interp t2 env

pgm :: Term
pgm = App
        (Lam "x" (Var "x" :+: Var "x"))
        (Con 10 :+: Con 11)

myTest = do
    test pgm