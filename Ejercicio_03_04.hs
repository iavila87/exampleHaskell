--3. Dado el tipo de datos
--data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
--a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
--Las funciones de acceso son headCL, tailCL, isEmptyCL,isCUnit.
--headCL y tailCL no estan definidos para una lista vacıa.
--headCL toma una CList y devuelve el primer elemento de la misma
--(el de mas a la izquierda).
--tailCL toma una CList y devuelve la misma sin el primer elemento.
--isEmptyCL aplicado a una CList devuelve True si la CList es vacıa 
--(EmptyCL) o False en caso contrario.
--isCUnit aplicado a una CList devuelve True si la CList tiene un solo 
--elemento (CUnit a) o False en caso contrario.
--b) Definir una funcion reverseCL que toma una CList y devuelve su inversa.
--c) Definir una funcion inits que toma una CList y devuelve una CList con 
--todos los posibles inicios de la CList.
--d) Definir una funcion lasts que toma una CList y devuelve una CList con
--todas las posibles terminaciones de la CList.
--e) Definir una funcion concatCL que toma una CList de CList y devuelve la 
--CList con todas ellas concatenadas
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL:: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL:: CList a -> CList a
tailCL (CUnit _) = EmptyCL  
tailCL (Consnoc x ys z) = (Consnoc (headCL ys) (tailCL ys) z)

isEmptyCL:: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL (Consnoc _ _ _) = False
isEmptyCL (CUnit _) = False

isCUnit:: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit EmptyCL = False
isCUnit (Consnoc _ _ _) = False

reverseCL:: CList a -> CList a
reverseCL EmptyCL = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x ys z) = (Consnoc z (reverseCL ys) x)

inits:: CList a -> CList (CList a)
--quitar [x] = []
--quitar (x:xs) = x: quitar xs
--ini [] = [[]]
--ini xs = (ini (quitar xs)) ++ [xs]
snoc' EmptyCL e = (CUnit e)
snoc' (CUnit y) e = (Consnoc y EmptyCL e)
snoc' (Consnoc x ys z) e = (Consnoc x (snoc' ys z) e)

cons' :: a ->CList a -> CList a 
cons' e EmptyCL = CUnit e
cons' e (CUnit y) = Consnoc e EmptyCL y 
cons' e (Consnoc x l c) = Consnoc e (cons' x l) c 

lastElem (CUnit x) = x
lastElem (Consnoc x ys z) = z  

quitLast' (CUnit x) = EmptyCL
quitLast' (Consnoc x EmptyCL z) = (CUnit x)
quitLast' (Consnoc x ys z) = (Consnoc x (quitLast' ys) (lastElem ys)) 

inits EmptyCL = (CUnit EmptyCL)
inits cl = snoc' (inits (quitLast' cl)) cl

quitFirst' (CUnit x) = EmptyCL
quitFirst' (Consnoc x EmptyCL z) = (CUnit z)
quitFirst' (Consnoc x ys z) = (Consnoc (headCL ys) (tailCL ys) z)

lasts EmptyCL = (CUnit EmptyCL)
lasts cl = snoc' (lasts (quitFirst' cl)) cl
--e) Definir una funcion concatCL que toma una CList de CList y devuelve la 
--CList con todas ellas concatenadas

--concatCL EmptyCL EmptyCL= EmptyCL
--concatCL EmptyCL cl = cl 
--concatCL cl EmptyCL = cl
--concatCL (CUnit x) (CUnit y) = Consnoc x EmptyCL y
--concatCL (Consnoc x EmptyCL z) (Consnoc a2 cl2 z2) = (Consnoc x ( cons' z (cons' a2 cl2)) z2)
--concatCL (Consnoc a l z)  (CUnit x) = (Consnoc a (snoc' l z) x)
--concatCL (CUnit x) (Consnoc a l z) = (Consnoc x (cons' a l) z)
--concatCL (Consnoc a x z) l = cons' a (Consnoc (snoc' l z) l)

--4. Dado el siguiente tipo algebraico:
--data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
--a) Defina un evaluador eval :: Aexp → Int. ¿C ́omo maneja los errores 
--de division por 0?
--b) Defina un evaluador seval :: Aexp → Maybe Int.

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp
eval:: Aexp -> Int
eval (Num x) = x
eval (Prod x y) = (eval x) * (eval y)
eval (Div _ (Num 0)) = error "division por 0"
eval (Div x y) = (eval x) `div` (eval y)

prodM Nothing _ = Nothing
prodM _ Nothing = Nothing
prodM (Just x) (Just y) = Just (x*y)

divM _ (Just 0) = Nothing
divM Nothing _ = Nothing
divM _ Nothing = Nothing
divM (Just x) (Just y) = Just (x`div`y)

seval:: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod (Num x) (Num y)) = (Just (x*y))
seval (Prod x y) = prodM (seval x) (seval y)
seval (Div _ (Num 0)) = Nothing
seval (Div x y) = divM (seval x) (seval y)