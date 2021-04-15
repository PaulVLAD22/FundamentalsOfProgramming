type M = Either String

--ultima chestie de la lab

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

showM :: Show a => M a -> String
showM (Left x) = "Error: " ++ show x
showM (Right x) = show x

add :: Value -> Value -> M Value
add (Num i) (Num j) = Right $ Num $ i + j
add (Num i) (Fun a) = Left $ "Tried to add " ++ show i ++ " to a function"
add (Fun a) (Num i) = Left $ "Tried to add a function to " ++ show i
add (Fun a) (Fun b) = Left "Tried to add two functions"

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply  v _ = Left $ "'" ++ show v ++ "' should be a function" 
-- apply _ _ = Left "ERROR"


lookupM :: Name -> Environment -> M Value
lookupM x env = case lookup x env of
    Just v -> Right v
    Nothing -> Left $ "Unbound variable: " ++ x

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

pgm :: Term
pgm = App
        (Lam "x" (Var "x" :+: Var "x"))
        (Con 10 :+: Con 11)

pgmG = App
    ((Var "x"):+: (Var "x")) ((Con 10) :+: (Con 11))
myTest = do
    test pgmG