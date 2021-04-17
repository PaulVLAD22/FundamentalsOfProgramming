type Name = String 

data Term = Var Name 
            | Con Integer 
            | Term :+: Term
            | Term :/: Term
            | Lam Name Term
            | App Term Term
            | Out Term
        deriving (Show)

newtype WriterS a = Writer { runWriter :: (a, String) } 
-- runWriter face din a monadic in a normal (scoate a din monada)
instance Monad WriterS where
  return va = Writer (va, "") --suprascriere return
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf --luam functia 
    a <- ma-- luam datele
    return (f a) -- returnam monada cu functia aplicata pe date

instance Functor WriterS where              
  fmap f ma = f <$> ma


newtype MaybeWriter a = MW {getvalue :: Maybe(a,String)}

instance Monad MaybeWriter  where
  return x = MW $ Just (x,"")
  ma >>= k = case a of 
                Nothing -> MW Nothing 
                Just (x,w) ->
                    case getvalue (k x) of 
                        Nothing -> MW Nothing 
                        Just (y,v) -> MW $ Just (y,w++v)
            where a = getvalue ma


instance Applicative MaybeWriter where
  pure = return
  mf <*> ma = do
    f <- mf --luam functia 
    a <- ma-- luam datele
    return (f a) -- returnam monada cu functia aplicata pe date

instance Functor MaybeWriter where              
  fmap f ma = f <$> ma



type M a = MaybeWriter a




showM ma = case a of 
                Nothing -> "Nothing"
                Just (x,w)-> "Output: " ++ w ++ "\nValue: " ++ Prelude.show x
            where a = getvalue ma

data Value = Num Integer
    | Fun (Value -> M Value)
    
test :: Term -> String
test t = showM $ interp t []

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

divide :: Value -> Value -> M Value
divide (Num i) (Num 0) = MW Nothing 
divide (Num i) (Num j) = return (Num $ i `div` j)
divide _ _ = MW Nothing

apply :: Value -> Value -> M Value
apply (Fun k) v = k v
apply _ _ = MW Nothing


tell :: String -> MaybeWriter  ()
tell log = MW $ Just ((),log)

pgmW2 = App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Con 10) :/: (Out (Con 0)))

