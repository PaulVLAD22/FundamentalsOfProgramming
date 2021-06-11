{-
Gasiti mai jos limbajul unui minicalculator si o interpretare partiala. 
Calculatorul are doua celule de memorie, care au valoarea initiala  0.
Instrucțiunea `mem :< expr` are urmatoarea semantica: 
`expr` este evaluata, iar valoarea este pusa in `mem`.  
Un program este o expresie de tip `Prog` care execută pe rănd toate instrucțiunile iar rezultatul executiei
este starea finală a memoriei. 
Testare se face apeland `prog test`. 
-}

data Prog  = On [Stmt]
data Stmt  = Mem :< Expr
data Mem = Mem1 | Mem2 
data Expr  =  M Mem | V Int | Expr :+ Expr | Expr :/ Expr
 

infixl 6 :+
infix 4 :<

type Env = (Int,Int)   -- corespunzator celor doua celule de memorie

type Env' = Either String Env
  
expr ::  Expr -> Env -> Int
expr (e1 :+  e2) (m1,m2) = (expr  e1 (m1,m2)) + (expr  e2 (m1,m2))
expr (e1 :/ e2) (m1,m2) = if expr e2 (m1,m2) /=0 then (expr e1 (m1,m2) `div` (expr e2 (m1,m2))) else error("Impartire la 0")
expr (V x) env = x
expr (M Mem1) (m1,m2) = m1
expr (M Mem2) (m1,m2) = m2

expr' ::  Expr -> Env -> Int
expr' (e1 :+  e2) (m1,m2) = (expr  e1 (m1,m2)) + (expr  e2 (m1,m2))
expr' (e1 :/ e2) (m1,m2) = if expr e2 (m1,m2) /=0 then (expr e1 (m1,m2) `div` (expr e2 (m1,m2))) else error ("Impartire la 0")
expr' (V x) env = x
expr' (M Mem1) (m1,m2) = m1
expr' (M Mem2) (m1,m2) = m2


stmt :: Stmt -> Env -> Env
stmt (Mem1 :< e) (m1,m2) = (expr e (m1,m2),m2) 
stmt (Mem2:< e) (m1,m2) = (m1,expr e (m1,m2))

stmt' :: Stmt -> Env' -> Env'
stmt' _ (Left string) = Left string
stmt' (Mem1 :< (e1 :/ e2)) (Right (m1,m2)) = if (expr' e2 (m1,m2) /=0) then Right (expr' (e1 :/ e2) (m1,m2),m2) else Left "Impartire la 0" 
stmt' (Mem1 :< e)(Right(m1,m2)) = Right (expr' e (m1,m2),m2)
stmt' (Mem2:< e) (Right (m1,m2)) = Right (m1,expr' e (m1,m2))




stmts :: [Stmt] -> Env -> Env
stmts (st : sts) (m1,m2) = stmts sts (stmt st (m1,m2))
stmts _ env = env 

stmts' :: [Stmt] -> Env' -> Env'
stmts' (st : sts) env = stmts' sts (stmt' st env)
stmts' _ env = env 

prog :: Prog -> Env
prog (On ss) = stmts ss (0, 0)

prog' :: Prog -> Env'
prog' (On ss) = stmts' ss (Right (0,0))


test1 = On [Mem1 :< V 3, Mem2 :< M Mem1 :+ V 5]
test2 = On [Mem2 :< V 3, Mem1 :< V 4, Mem2 :< (M Mem1 :+ M Mem2) :+ V 5]
test3 = On [Mem2 :< V 3 :+  V 3]

test4 = On [Mem1 :< V 3 :/ V 3]

test5 = On [Mem1 :< V 3 :/ V 0]
{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10 pct) Adaugati expresia `e1 :/ e2` care evaluează expresiile e1 și e2, apoi
  - dacă valoarea lui e2 e diferită de 0, se evaluează la câtul împărțirii lui e1 la e2;
  - în caz contrar va afișa eroarea "împarțire la 0" și va încheia execuția.
3)(20pct) Definiti interpretarea  limbajului extins astfel incat interpretarea unui program / instrucțiune / expresie
   să aibă tipul rezultat `Either String Env`/ `Either String Int`, unde rezultatul final în cazul în care execuția programului
   întâlnește o împărțire la 0 va fi `Left "împarțire la 0"`.  
   Rezolvați subiectul 3) în același fișier redenumind funcțiile de interpretare. 


Indicati testele pe care le-ati folosit in verificarea solutiilor. 

-}