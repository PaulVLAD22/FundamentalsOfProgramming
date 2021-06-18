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

instance Monad Writer where
  return a = Writer (a, "")
  ma >>= k = let (a, log1) = runWriter ma
                 (b, log2) = runWriter (k a)  
              in Writer (b, log1 ++ log2)
--ex 1 
instance Monad MaybeWriter where
  return a = MW $ Just (a,"")
  ma >>= k = case getvalue ma of 
    Nothing -> error "eroare"
    Just (a,log1) -> case getvalue (k a) of 
      Nothing -> error "eroare"
      Just (b,log2) -> MW $ Just(b,log1 ++ log2)

instance Applicative Writer where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }


instance Applicative MaybeWriter where
  pure = return
  mf <*> ma = do { f <- mf; a <- ma; return (f a) }

instance Functor Writer where
  fmap f ma = pure f <*> ma

instance Functor MaybeWriter where
  fmap f ma = pure f <*> ma

type M a = MaybeWriter a

showM :: Show a => M a -> String
showM ma = case getvalue ma of 
  Just (a,w)-> "Output: " ++ w ++ "\nValue: " ++ show a
  Nothing -> "Nothing"

data Value = Num Integer
  | Fun (Value -> M Value)

type Environment = [(Name, Value)]
instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"
  --show Wrong = "<wrong>"

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
  mult v1 v2 

interp (Lam x e) env =
  return $ Fun $ \ v -> interp e ((x,v):env)

interp (App t1 t2) env = do
  f <- interp t1 env
  v <- interp t2 env
  apply f v

interp (Out t) env = do
  v <- interp t env
  tell (show v ++ "; ")
  return v

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = MW Nothing  


tell :: String -> MaybeWriter ()
tell log =  MW $ Just ((), log)

get :: Name -> Environment -> M Value
get x env = case [v | (y,v) <- env , x == y] of
  (v:_) -> return v
  _ -> MW Nothing 

add :: Value -> Value -> M Value
add (Num 0) (Num j ) = MW Nothing 
add (Num i) (Num j) = return (Num $ i + j)
add _ _ = MW Nothing 

mult :: Value -> Value -> M Value
mult (Num i ) (Num j) = return (Num$ i*j)
mult _ _ = MW Nothing 

test :: Term -> String
test t = showM $ interp t []


pgm = App
  (Lam "x" ((Var "x") :+: (Var "x")))
  ((Out (Con 10)) :+: (Out (Con 11)))

pgmW4 = App (Var "y") (Lam "y" (Out (Con 3)))
