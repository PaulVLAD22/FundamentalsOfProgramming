{-
Gasiti mai jos limbajul unui minicalculator si o interpretare partiala. 
Calculatorul are doua celule de memorie. Interpretarea instructiunilor este data mai jos. 

Un program este o expresie de tip Progiar rezultatul executiei este starea finala a memoriei
Testare se face apeland prog test. 
-}

type Env = (Int,Int)   -- corespunzator celor doua celule de memorie

type Env' = (Int,Int,Int) 

data Prog  = On Env Stmt  -- Env reprezinta valorile initiale ale celulelor de memorie
data Stmt
    = Off
    | Expr :<< Stmt -- evalueaza Expr, pune rezultatul in Mem1, apoi executa Stmt
    | Expr :< Stmt  -- evalueaza Expr, pune rezultatul in Mem2, apoi executa Stmt
    
data Mem = Mem1 | Mem2 
data Expr  =  M Mem | V Int | Expr :+ Expr | If1 Expr Expr | If2 Expr Expr

infixl 6 :+
infixr 2 :<
infixr 2 :<<


expr ::  Expr -> Env -> Int
expr (e1 :+  e2) m = expr e1 m + expr e2 m
expr (M Mem1) (m1,m2) = m1 
expr (M Mem2) (m1,m2) = m2 
expr (V i) env = i 
expr (If1 e1 e2) (m1,m2) = if (m1==0) then expr e2 (m1,m2) else expr e1 (m1,m2)
expr (If2 e1 e2) (m1,m2) = if (m2==0) then expr e2 (m1,m2) else expr e1 (m1,m2)

stmt :: Stmt -> Env -> Env
stmt Off m = m
stmt (e :<< st) (m1,m2) = stmt st (expr e (m1,m2),m2)
stmt (e :< st) (m1,m2) = stmt st (m1,expr e (m1,m2))


stmt' :: Stmt -> Env' -> Env'
stmt' Off m = m
stmt' (M Mem1 :<< st) (m1,m2,m3) = stmt' st (expr (M Mem1) (m1,m2),m2,m3+2)
stmt' (M Mem2 :<< st) (m1,m2,m3) = stmt' st (expr (M Mem2) (m1,m2),m2,m3+2)
stmt' (e :<< st) (m1,m2,m3) = stmt' st (expr e (m1,m2),m2,m3+1)
stmt' (e :< st) (m1,m2,m3) = stmt' st (m1,expr e (m1,m2),m3+1)



prog :: Prog -> Env
prog (On m s) = stmt s m 

prog' :: Prog -> Env'
prog' (On (m1,m2) s) = stmt' s (m1,m2,0) 

test1 = On (1,2) (V 3 :< M Mem1 :+ V 5 :<< Off)
test2 = On (0,0) (V 3 :<< M Mem2 :+ V 5 :< Off)
test3 = On (0,1) (V 3 :<< V 4 :< M Mem1 :+ M Mem2 :+ (V 5) :< Off)
test4 = On (-2,3)(M Mem1  :+  V 3 :< Off)

-- verificare 2)
test5 = On (1,2) (V 3 :< M Mem1 :+ V 5 :<< If1 (V 10) (V 15) :< Off)

{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10 pct) Adaugati expresiile If1 e1 e2 si If2 e1 e2  care evaluează  e1 daca Mem1, 
respectiv Mem2, este nenula sie2 in caz contrar.
3) (20pct) Definiti interpretarea  limbajului extins  astfel incat executia unui program  sa calculeze memoria finala,
  si numărul de accesări (scrieri și citiri) ale memoriilor Mem1 si Mem2 (se va calcula o singura
  valoare, insumand accesarile ambelor memorii, fara a lua in considerare initializarea). 
  Rezolvați subiectul 3) în același fișier, redenumind funcțiile de interpretare. 


Indicati testele pe care le-ati folosit in verificarea solutiilor. 

-}