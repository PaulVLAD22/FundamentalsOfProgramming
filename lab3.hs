-- I

{- Monada Maybe este definita in GHC.Base 
instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing
instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       
instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

-- nu scriem moanda maybe pt ca e deja definita
-- in lab 3/4 poti vedea definitia ei
(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = \x -> g x >>= f

-- <=< face compunerea functiilor

f :: Int -> Maybe String
f a
    | a >= 0 = Just "Pozitiv"
    | otherwise = Nothing

g :: Bool -> Maybe Int
g b
    | b = Just 1
    | otherwise = Nothing

h :: String -> Maybe Bool
h a
    | a == "Pozitiv" = Just True
    | otherwise = Nothing

ex1, ex2, ex3 :: Maybe String
ex1 = (f <=< g) True
ex2 = (f <=< g) False
ex3 = (f <=< g <=< h) "Pozitiv"

f' :: Int -> Maybe Int
f' a
    | a > 0 = Just $ a * 6
    | otherwise = Nothing

g' :: Int -> Maybe Int
g' a
    | a > 8 = Just $ a `div` 2
    | otherwise = Nothing

h' :: Int -> Maybe Int
h' a
    | even a = Just 72
    | otherwise = Nothing
-- cele de mai sus sunt scrise ca sa le folosim aici

-- asociativitate
asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = (h <=< (g <=< f) $ x) == ((h <=< g) <=< f $ x)

-- q = quickCheck $ asoc f' g' h'

pos :: Int -> Bool
pos  x = x >= 0

foo :: Maybe Int ->  Maybe Bool
foo mx = mx >>= \x -> Just $ pos x

fooDo :: Maybe Int -> Maybe Bool
fooDo mx = do
    x <- mx --scoatem valoarea din cutie
    let b = pos x
    return b --punem valoarea in cutie

fooProp :: Maybe Int -> Bool
fooProp x = (fooDo x) == (foo x) 
-- fara monade
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM Nothing _ = Nothing 
addM _ Nothing = Nothing
addM (Just x) (Just y) = Just(x+y)
-- cu monade
addM2:: Maybe Int -> Maybe Int -> Maybe Int
addM2 mx my = do 
                x <- mx
                y <- my 
                return (x+y)


cartesianProduct xs ys = xs >>= \x -> ys >>= \y -> return (x, y)
cartesianProductDo xs ys = do
    x <- xs
    y <- ys
    return (x, y)

testCartesianProduct :: [Int] -> [Int] -> Bool
testCartesianProduct x y = cartesianProduct x y == cartesianProductDo x y

-- q'' = quickCheck testCartesianProduct


prod f xs ys = [f x y | x <- xs, y <- ys]
prodDo f xs ys = do
    x <- xs
    y <- ys
    return (f x y)

testProd :: (Int -> Int -> Int) -> [Int] -> [Int] -> Bool
testProd f x y = prod f x y == prodDo f x y

-- q''' = quickCheck $ testProd $ \x y -> x + y + 1


myGetLine :: IO String
myGetLine = getChar >>= \x ->
    if x == '\n' then
        return []
    else
        myGetLine >>= \xs -> return (x:xs)

myGetLineDo :: IO String
myGetLineDo = do
    y <- getChar
    if y == '\n' then
        return []
    else do
        z <- myGetLineDo
        return (y : z)


prelNo = sqrt

ioNumber = do
    noin <- readLn :: IO Float
    putStrLn $ "Intrare\n" ++ show noin
    let noout = prelNo noin
    putStrLn "Iesire"
    print noout

-- >> are rol de ;
--n am inteles partea asta exact
ioNumberSeq = (readLn :: IO Float)
                >>= \noin -> putStrLn ("Intrare\n" ++ show noin)
                >> putStrLn "Iesire"
                >> print (prelNo noin)




-- II


-- mai fa exercitiul in care faci lista de stringuri in loc de concatenare de stringuri

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

tell :: String -> WriterS () 
tell log = Writer ((), log)
--adauga in al doilea element
  
logIncrement :: Int  -> WriterS Int
logIncrement x = do 
                    tell ("increment: " ++ show x ++ "\n") -- adauga la monada
                    return (x+1) --return ul e suprascris mai sus

logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n
    | n > 0 = do
        newX <- logIncrement x --incrementam valoarea din contextul monadei
        logIncrementN newX $ n - 1 
    | otherwise = return x

isPos :: Int -> WriterS Bool
isPos x = if x >= 0 then Writer (True, "poz\n") else Writer (False, "neg\n")                         

mapWriterS :: (a -> WriterS b) -> [a] -> WriterS [b]
mapWriterS f xs
    | null xs = Writer (mempty, mempty)
    | otherwise = do
        let m = map f xs
        let z = map runWriter m
        Writer (map fst z, concatMap snd z)

mapWriterSTest :: ([Bool], String)
mapWriterSTest = runWriter $ mapWriterS isPos [-2, 2, -1, 1, 0]

newtype WriterLS a = Writer' { runWriter' :: (a, [String]) } 

instance Monad WriterLS where
  return va = Writer' (va, [])
  ma >>= k = let (va, log1) = runWriter' ma
                 (vb, log2) = runWriter' (k va)
             in  Writer' (vb, log1 ++ log2)


instance Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    f <$> ma      

instance Functor WriterLS where              
  fmap f ma = f <$> ma


logIncrement' :: Int -> WriterLS Int
logIncrement' x = Writer'(x + 1, ["Increment: " ++ show x])

logIncrementN' :: Int -> Int -> WriterLS Int
logIncrementN' x n
    | n > 0 = do
        newX <- logIncrement' x
        logIncrementN' newX $ n - 1
    | otherwise = Writer'(x, [])

isPos' :: Int -> WriterLS Bool
isPos' x = if x >= 0 then do 
        return True
    else do
        return False                      

mapWriterLS :: (a -> WriterLS b) -> [a] -> WriterLS [b]
mapWriterLS f xs
    | null xs = Writer' (mempty, mempty)
    | otherwise = do
        let m = map f xs
        let z = map runWriter' m
        Writer' (map fst z, map (head . snd) z)

mapWriterLSTest = runWriter' $ mapWriterLS isPos' [-2, 2, -1, 1, 0]


-- III

--monada reader 

newtype Reader env a = Reader { runReader :: env -> a }

instance Monad (Reader env) where
  return x = Reader (const x)
  ma >>= k = Reader f --ma e de tip Reader env
    where f env = let a = runReader ma env 
                  in  runReader (k a) env -- aplica functia de evaluare k pe valoarea returnata a in functie de mediul de evaluare


instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    f <$> ma

instance Functor (Reader env) where
  fmap f ma = f <$> ma


ask :: Reader env env
ask = Reader id

local :: (r -> r) -> Reader r a -> Reader r a
local f ma = Reader $ runReader ma . f -- runreader scoate 
                                       -- reader pune inapoi in cutie

-- Reader Person String

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN p = "NAME: " ++ name p

showPersonA :: Person -> String
showPersonA p= "AGE: " ++ show (age p) 

showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")"

mshowPersonN :: Reader Person String
mshowPersonN = do
    p <- ask -- pun person in p (Scot din cutia reader)
    return $ "NAME: " ++ name p

mshowPersonN' :: Reader Person String
mshowPersonN' = ask >>= \env -> Reader (const $ "NAME: " ++ name env)

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