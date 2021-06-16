
newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)
  
logIncrement :: Int  -> WriterS Int
logIncrement x = Writer (x+1,"incremented "++show x)

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x 0 = return x
logIncrementN x n = do 
   tell("Incremented "++ show x)
   logIncrementN (x+1) (n-1)




newtype Reader env a = Reader { runReader :: env -> a }

instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor (Reader env) where
  fmap f ma = pure f <*> ma


ask :: Reader env env
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f ma = Reader $ (\r -> (runReader ma)(f r))

-- Reader Person String

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN p = name p

showPersonA :: Person -> String
showPersonA p = show $ age p


showPerson :: Person -> String
showPerson p = showPersonN p ++ showPersonA p

mshowPersonN :: Reader Person String
mshowPersonN = do
    p <- ask -- pun person in p (Scot din cutia reader)
    return $ "NAME: " ++ name p

mshowPersonA :: Reader Person String
mshowPersonA = do
    env <- ask
    return $ "AGE: " ++ show (age env)

mshowPerson :: Reader Person String
mshowPerson = do
    env <- ask
    let name = runReader mshowPersonN env
        age = runReader mshowPersonA env
    return $ "(" ++ name ++ ", " ++ age ++ ")"


(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\ x -> g x >>= f)

asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = undefined 

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

foo :: Maybe Int ->  Maybe Bool 
foo  mx =  mx  >>= (\x -> Just (pos x))  
  
addM :: Maybe Int -> Maybe Int -> Maybe Int  
addM mx my = do 
  x <- mx 
  y <- my 
  return (x+y)

-- probleme la reader si maybe

