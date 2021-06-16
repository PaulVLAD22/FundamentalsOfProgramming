
newtype EnvReader a = Reader { runEnvReader :: Environment -> a }


instance Show a => Show (EnvReader a) where
    show x = show $ runEnvReader x []

instance  Monad EnvReader where
  return va = Reader $ const va
  ma >>= k = Reader $ \r -> runEnvReader (k $ runEnvReader ma r) r
 
instance  Applicative EnvReader where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor EnvReader where              
  fmap f ma = pure f <*> ma    

type M = EnvReader


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
  deriving (Show)



data Value = Num Integer
           | Fun (Value -> M Value)
           | Wrong

instance Show Value where
 show (Num x) = show x
 show (Fun _) = "<function>"
 show Wrong   = "<wrong>"

showM :: Show a => M a -> String
showM ma = show $ runEnvReader ma []

type Environment = [(Name, Value)]

ask :: EnvReader Environment
ask = Reader id -- id e functia identitate,

local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma  = Reader $ (\r -> (runEnvReader ma) (f r))
--

interp :: Term -> M Value
interp (Var name) =do
    env <- ask
    case lookup name env of
        Just v -> return v
        Nothing -> return Wrong

interp (Con i) = return  $ Num i 
interp (ma :+: mb) = do
  a <- interp ma 
  b <-interp mb
  case (a,b) of 
    (Num i,Num j) -> return $ Num (i+j)
    (_,_) -> return Wrong
interp (Lam x e) = return $ Fun (\v -> interp e)
interp (App t1 t2 ) = do 
  m1 <- interp t1 
  m2 <- interp t2 
  case (m1,m2) of 
    (Fun x,y) -> x y
    (_,_) -> return Wrong 
-- test :: Term -> String
-- test t = showM $ interp t []

pgm1:: Term
pgm1 = App
          (Lam "x" ((Var "x") :+: (Var "x")))
          ((Con 10) :+:  (Con 11))

term0 = (App (Lam "x" (Var "x" :+: Var "x")) (Con 10 :+: Con 11))
-- test pgm
-- test pgm1

            

-- nu afiseza bine 