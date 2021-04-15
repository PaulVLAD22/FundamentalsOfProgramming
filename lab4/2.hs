type M = Maybe

--maybe

type Name = String
data Term = Var Name
            | Con Integer
            | Term :+: Term
            | Lam Name Term
            | App Term Term
            
    deriving (Show)

data Value = Num Integer
            | Fun (Value -> M Value)

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"

type Environment = [(Name, Value)]

--

test :: Term -> String
test t = showM $ interp t []

lookUpM ::Name -> Environment -> M Value
lookUpM x env = case lookup x env of
        Just v -> return v
        Nothing -> Nothing


showM :: Show a => M a -> String
showM (Just x) = show x
showM Nothing = "Error"

add :: Value -> Value -> M Value
add (Num i) (Num j) = return $ Num $ i + j
add _ _ = Nothing

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = Nothing

--

interp :: Term -> Environment -> M Value
interp (Var x) env = lookup x env
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

pgm :: Term
pgm = App
        (Lam "x" (Var "x" :+: Var "x"))
        (Con 10 :+: Con 11)

pgm2 :: Term
pgm2 = App
        (Lam "x" ((Var "x") :+: (Var "y")))
        ((Con 10) :+: (Con 11))

myTest = do
    test pgm

myTest2 = do
    test pgm2