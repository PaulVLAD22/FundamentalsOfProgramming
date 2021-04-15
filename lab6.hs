module Checker where


import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SIMPLE

data Type = TInt | TBool
  deriving (Eq)

instance Show Type where
    show TInt = "int"
    show TBool = "bool"

type CheckerState = Map Name Type

emptyCheckerState :: CheckerState
emptyCheckerState = Map.empty

newtype EReader a =
    EReader { runEReader :: CheckerState ->  (Either String a) }

throwError :: String -> EReader a
throwError e = EReader (\_ -> (Left e))

instance Monad EReader where
    return a = EReader (\env ->  Right a)
    act >>= k = EReader  f
                where
                 f env  = case (runEReader act env) of
                           Left s -> Left s
                           Right va -> runEReader (k va) env


instance Functor EReader where
    fmap f ma = do { a <- ma; return (f a) }

instance Applicative EReader where
    pure = return
    mf <*> ma = do { f <- mf; a <- ma; return (f a)}

askEReader :: EReader CheckerState
askEReader =EReader (\env -> Right env)

localEReader :: (CheckerState -> CheckerState) -> EReader a -> EReader a
localEReader f ma = EReader (\env -> (runEReader ma) (f env))


type M = EReader

expect :: (Show t, Eq t, Show e) => t -> t -> e -> M ()
expect tExpect tActual e =
    if (tExpect /= tActual)
    then     (throwError
        $ "Type mismatch. Expected " <> show tExpect <> " but got " <> show tActual
        <> " for " <> show e)
    else (return ())

lookupM :: Name -> M Type
lookupM name = do
    env <- askEReader
    case Map.lookup name env of
        Nothing -> throwError $ "Variable " <> name <> " not declared."
        Just ans -> return ans

checkExp :: Exp -> M Type
checkExp (I _) = return TInt
checkExp (B _) = return TBool
checkExp (Id x) = lookupM x
checkExp (UMin exp) = checkExp exp

checkExp (BinA _ exp1 exp2) = do
    t1 <- checkExp exp1
    expect TInt t1 exp1
    t2 <- checkExp exp2
    expect TInt t2 exp1
    return TInt

checkExp (BinC _ exp1 exp2) = do
    t1 <- checkExp exp1
    expect TInt t1 exp1
    t2 <- checkExp exp2
    expect TInt t2 exp2
    return TBool

checkExp (BinE _ exp1 exp2) = do
    t1 <- checkExp exp1
    expect TInt t1 exp1
    t2 <- checkExp exp2
    expect TInt t2 exp2
    return TBool

checkExp (BinL _ exp1 exp2) = do
    t1 <- checkExp exp1
    expect TBool t1 exp1
    t2 <- checkExp exp2
    expect TBool t2 exp2
    return TBool

checkExp (Not exp) = do
    t <- checkExp exp
    expect TBool t exp
    return TBool


checkStmt :: Stmt -> M ()
checkStmt (Decl _ _) = return ()
checkStmt (Block block) = checkBlock block

checkStmt (Asgn name exp) = do
    t1 <- lookupM name
    t2 <- checkExp exp
    expect t1 t2 exp

checkStmt (If cond s1 s2) = do
    t1 <- checkExp cond
    expect TBool t1 cond
    checkStmt s1
    checkStmt s2

checkStmt (While exp _) = do
    t <- checkExp exp
    expect TBool t exp

checkBlock :: [Stmt] -> M ()
checkBlock [] = return ()
checkBlock (Decl name exp : rest) = do
    t <- checkExp exp
    localEReader (Map.insert name t) (checkBlock rest)
checkBlock (x : y) = checkStmt x >> checkBlock y

checkPgm :: [Stmt] -> Bool
checkPgm pgm =
    case  runEReader (checkBlock pgm) emptyCheckerState of
        Left err -> error err
        Right _ -> True

checkStmtBool :: Stmt -> Bool
checkStmtBool stmt =
    case runEReader (checkStmt stmt) emptyCheckerState of
        Left err -> error err
        Right _ -> True