newtype EnvReader a = Reader { runEnvReader :: Environment -> a }

instance Show a => Show (EnvReader a) where
    show x = show $ runEnvReader x []

instance Functor EnvReader where
    fmap f (Reader x) = Reader $ f . x

instance Applicative EnvReader where
    pure = return
    mf <*> ma = do
        f <- mf
        f <$> ma

instance Monad EnvReader where
    return a = Reader $ const a
    m >>= k = Reader $ \r -> runEnvReader (k $ runEnvReader m r) r

type Name = String
type Environment = [(Name, Value)]

type M a = EnvReader a

-- final lab6 moanga

ask :: EnvReader Environment
ask = Reader id -- id e functia identitate,

local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma  = Reader $ (\r -> (runEnvReader ma) (f r))
--

data Term = Var Name
            | Con Integer
            | Term :+: Term
            | Lam Name Term
            | App Term Term

data Value = Num Integer
            | Fun (Value -> M Value)
            | Wrong

instance Show Value where
    show (Num x) = show x
    show (Fun _) = "<function>"
    show Wrong = "<wrong>"

-- helpers

showM :: Show a => M a -> String
showM ma = show $ runEnvReader ma []

lookupM :: Name -> M Value
lookupM x = do
    env <- ask
    case lookup x env of
        Just v -> return v
        Nothing -> return Wrong

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j)
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = return Wrong

-- interpretor

interp :: Term -> M Value
interp (Var x) = lookupM x
interp (Con i) = return $ Num i
interp (Lam x e) = do
    env <- ask
    return $ Fun $ \v -> local (const ((x, v) : env)) (interp e)
                            -- const ia x din env
interp (t1 :+: t2) = do
    v1 <- interp t1
    v2 <- interp t2
    add v1 v2

interp (App t1 t2) = do
    f <- interp t1
    v <- interp t2
    apply f v

pgm :: Term
pgm = App
        (Lam "x" (Var "x" :+: Var "x"))
        (Con 10 :+: Con 20)