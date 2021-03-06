
newtype StringWriter a = StringWriter { runStringWriter :: (a, String) }

instance  Monad StringWriter where
  return va = StringWriter (va,"")
  ma >>= k = let (va, log1) = runStringWriter ma
                 (vb, log2) = runStringWriter (k va)
              in  StringWriter (vb, log1 ++ log2)
 
instance  Applicative StringWriter where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor StringWriter where              
  fmap f ma = pure f <*> ma    

type M = StringWriter

instance Show a => Show (StringWriter a) where
    show ma = "Output: " ++ w ++ " Value: " ++ show a
        where (a, w) = runStringWriter ma

tell :: String -> StringWriter ()
tell x = StringWriter((),x) 

pgm :: Term
pgm = App
  (Lam "y"
    (App
      (App
        (Lam "f"
          (Lam "y"
            (App (Var "f") (Var "y"))
          )
        )
        (Lam "x"
          (Var "x" :+: Var "y")
        )
      )
      (Con 3)
    )
  )
  (Con 4)

type Name = String
data Term = Var Name
  | Con Integer
  | Term :+: Term
  | Lam Name Term
  | App Term Term
  | Out Term
  deriving (Show)



data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

type Environment = [(Name, Value)]




interp :: Term -> Environment -> M Value
interp (Var name) env = case lookup name env of
  Nothing-> return Wrong
  Just x -> return x 
interp (Con i) env = return $ Num i 
interp (ma :+: mb) env = do
  a <- interp ma env
  b <-interp mb env
  case (a,b) of 
    (Num i,Num j) -> return $ Num (i+j)
    (_,_) -> return Wrong
interp (Lam x e) env = return $ Fun (\v -> interp e ((x,v):env))
interp (App t1 t2 ) env = do 
  m1 <- interp t1 env
  m2 <- interp t2 env
  case (m1,m2) of 
    (Fun x,y) -> x y
    (_,_) -> return Wrong 
interp (Out t1) env = do 
                      x <- interp t1 env 
                      StringWriter(x,show x++";") 
-- test :: Term -> String
-- test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))
-- test pgm
-- test pgm1