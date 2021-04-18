type Name = String

data Term = Var Name
  | Con Integer
  | Term :+: Term
  | Term :/: Term
  | Lam Name Term
  | App Term Term
  | Out Term
  deriving (Show)

newtype Writer a = Writer { runWriter :: (a, String) }

newtype MaybeWriter a = MW {getvalue :: Maybe (a,String)}

instance Monad MaybeWriter where
  return a = MW $ Just (a,"")
  ma >>=k = let x = getvalue ma
            in case x of 
              Nothing -> MW Nothing 
              Just (a,log1) -> case getvalue (k a) of 
                Nothing -> MW Nothing 
                Just (b,log2) -> MW $ Just (b,log1 ++ log2)

instance Monad Writer where
  return a = Writer (a, "")
  ma >>= k = let (a, log1) = runWriter ma
                 (b, log2) = runWriter (k a)
             in Writer (b, log1 ++ log2)

instance Applicative MaybeWriter where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }

instance Functor MaybeWriter where
  fmap f ma = pure f <*> ma

instance Applicative Writer where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }

instance Functor Writer where
  fmap f ma = pure f <*> ma

type M a = MaybeWriter a

data Value = Num Integer
  | Fun (Value -> M Value)

showM :: Show a => M a -> String
showM ma = case getvalue ma of
  Nothing -> "Nothing"
  Just (a,w) -> "Output: " ++ w ++ "\nValue: " ++ Prelude.show a


type Environment = [(Name, Value)]

instance Show Value where
show (Num x) = Prelude.show x
show (Fun _) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) env = get x env
interp (Con i) _ = return $ Num i
interp (t1 :+: t2) env = do
  v1 <- interp t1 env
  v2 <- interp t2 env
  add v1 v2

interp (t1 :/: t2) env = do 
  v1 <- interp t1 env
  v2 <- interp t2 env
  divide v1 v2

interp (Lam x e) env =
  return $ Fun $ \ v -> interp e ((x,v):env)

interp (App t1 t2) env = do
  f <- interp t1 env
  v <- interp t2 env
  apply f v

interp (Out t) env = do
  v <- interp t env
  tell (Prelude.show v ++ "; ")
  return v

get :: Name -> Environment -> M Value
get x env = case [v | (y,v) <- env , x == y] of
  (v:_) -> return v
  _ -> MW Nothing

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num $ i + j)
add _ _ = MW Nothing

divide :: Value -> Value -> MaybeWriter Value
divide (Num i) (Num j) = return (Num $ i + j)
divide _ _ = MW Nothing


apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = MW Nothing

tell :: String -> MaybeWriter ()
tell log = MW $ Just ((), log)


pgm = App
  (Lam "x" ((Var "x") :+: (Var "x")))
  ((Out (Con 10)) :+: (Out (Con 11)))

test :: Term -> String
test t = showM $ interp t []