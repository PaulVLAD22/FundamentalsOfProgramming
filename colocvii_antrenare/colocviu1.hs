{-
Gasiti mai jos limbajul unui minicalculator si o interpretare partiala. 
Calculatorul are doua celule de memorie, care au valoarea initiala 0. Expresia `Mem := Expr` are urmatoarea semantica: 
`Expr` este evaluata, iar valoarea este pusa in `Mem`.  
Un program este o expresie de tip `Prog`iar rezultatul executiei este dat de valorile finale ale celulelor de memorie.
Testare se face apeland `run test`. 
-}


-- ai de facut c


data Prog  = Stmt ::: Prog | Off
data Stmt  = Mem := Expr
data Mem = Mem1 | Mem2 
data Expr  =  M Mem | V Int | Expr :+ Expr | Expr :/ Expr

infixl 6 :+
infix 3 :=
infixr 2 :::

type Env = (Int,Int)   -- corespunzator celor doua celule de memorie (Mem1, Mem2)
type Env' = (Int,Int,String)
  
expr ::  Expr -> Env -> Int
expr (e1 :+  e2) m = expr  e1 m + expr  e2 m
expr (e1 :/ e2) m = if (expr e2 m /= 0) then expr e1 m `div` expr e2 m else error "Impartire la 0"
expr (V x) env = x 
expr (M Mem1) (mem1,mem2) = mem1
expr (M Mem2) (mem1,mem2) = mem2


stmt :: Stmt -> Env -> Env
stmt (Mem1 := e) (v1,v2) = (expr e (v1,v2),v2)
stmt (Mem2 := e) (v1,v2) = (v1, expr e (v1,v2))

stmt' :: Stmt -> Env' -> Env'
stmt' (Mem1 := e) (v1,v2,s) = (expr e (v1,v2),v2,s++"Celula Mem1 a fost modificata cu " ++ show (expr e (v1,v2))++", ")
stmt' (Mem2 := e) (v1,v2,s) = (v1, expr e (v1,v2),s++"Celula Mem2 a fost modificata cu " ++ show (expr e (v1,v2))++", ")



prog :: Prog -> Env -> Env
prog Off m = m
prog (st ::: Off) env = stmt st env
prog (st ::: p) env = prog p (stmt st env) 

prog' :: Prog -> Env' ->Env'
prog' Off m = m
prog' (st ::: Off) env = stmt' st env
prog' (st ::: p) env = prog' p (stmt' st env) 

run :: Prog -> Env
run p = prog p (0, 0)

run' :: Prog -> Env'
run' p = prog' p (0,0,"")

test1 = Mem1 := V 3 ::: Mem2 := M Mem1 :+ V 5 ::: Off
test2 = Mem2 := V 3 ::: Mem1 := V 4 ::: Mem2 := (M Mem1 :+ M Mem2) :+ V 5 ::: Off
test3 = Mem1 := V 3 :+  V 3 ::: Off

{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10 pct) Adaugati expresia `e1 :/ e2` care evaluează expresiile e1 și e2, apoi
  - dacă valoarea lui e2 e diferită de 0, se evaluează la câtul împărțirii lui e1 la e2;
  - în caz contrar va afișa eroarea "împarțire la 0" și va încheia execuția.
3)(20pct) Definiti interpretarea  limbajului extins astfel incat executia unui program fara erori sa intoarca valoarea finala si un mesaj
   care retine toate modificarile celulelor de memorie (pentru fiecare instructiune `m :< v` se adauga 
   mesajul final `Celula m a fost modificata cu valoarea v`), mesajele pastrand ordine de efectuare a instructiunilor.  
    Rezolvați subiectul 3) în același fișier redenumind funcțiile de interpretare. 

Indicati testele pe care le-ati folosit in verificarea solutiilor. 

-}