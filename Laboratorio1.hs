module Practica0 where

import Data.List

{-
1) Los siguientes códigos tienen errores, cargar el archivo Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
        True -> "Quedate en Casa"
        False -> "Quedate en Casa"

-- b)
case' [x]         =  []
case' (x:y:xs)    =  y : case' (x:xs)
case' []          =  []

-- c)
map' f []        =  []
map' f (x:xs)     =  f x : map' f xs

-- d)
listNumeros = 1 : 2 : 3 : []

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)
addToTail x xs = map (+x) (tail xs)

-- g)
listmin xs = head (sort xs)

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs


--2. Definir las siguientes funciones y determinar su tipo:

--a) five, que dado cualquier valor, devuelve 5
five:: a -> Int
five _ = 5

--b) apply, que toma una función y un valor, y devuelve el resultado de
--aplicar la función al valor dado
apply:: (a->b) -> a -> b
apply f x = f x

--c) identidad, la función identidad
identidad:: a -> a
identidad x = x

--d) first, que toma un par ordenado, y devuelve su primera componente
first::(a,b)->a
first (x,_) = x

--e) derive, que aproxima la derivada de una función dada en un punto dado
fx2:: Num a => a->a
fx2 x = x*x

--derive:: Double -> Double-> (Double->Double) -> Double
derive h x f = (f (x+h) - f x) / h

--f) sign, la función signo
sign::(Ord a, Num a, Num p) => a -> p
sign x   | x<0 = -1
         | x==0 = 0
         | x>0 = 1

--g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs:: (Ord a, Num a) => a -> a
vabs x  | x < 0 = (-1)*x
        | otherwise = x

vabsS:: (Ord a, Num a) => a -> a
vabsS x = (sign x)*x

--h) pot, que toma un entero y un número, y devuelve el resultado de
--elevar el segundo a la potencia dada por el primero
pot:: (Ord a, Num a) => a->a->a
pot 1 y = y
pot x y = y*pot (x-1) y

--i) xor, el operador de disyunción exclusiva
xor:: Bool->Bool->Bool
xor x y | x == y = False
        | otherwise = True

--j) max3, que toma tres números enteros y devuelve el máximo entre los
max3:: Ord a => a ->a->a->a
max3 x y z = if (x > y) && (x > z) then x
                                else if (y > x) && (y > z) then y
                                else z

--k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap:: (a,b) -> (b,a)
swap (x,y) = (y,x)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

{-
4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int -> Int) -> Int
b) Int -> (Int -> Int)
c) (Int -> Int) -> (Int -> Int)
d) Int -> Bool
e) Bool -> (Bool -> Bool)
f) (Int,Char) -> Bool
g) (Int,Int) -> Int
h) Int -> (Int,Int)
i) a -> Bool
j) a -> a
-}



--5) Definir las siguientes funciones usando listas por comprensión:

--a) 'divisors', que dado un entero positivo 'x' devuelve la
--lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
divisors x = [n | n <- [1..x], x `mod` n == 0, x>0] 

--b) 'matches', que dados un entero 'x' y una lista de enteros descarta
--de la lista los elementos distintos a 'x'
matches x xs = [n | n <- xs, n == x]

--c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
--'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
--donde 0 <= a, b, c, d <= 'n'
cuadrupla n = [(a,b,c,d)|a<-[0..n],b<-[0..n],c<-[0..n],d<-[0..n],a^2+b^2==c^2+d^2]

--d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
--'xs' sin elementos repetidos
--unique :: [Int] -> [Int]
unique xs = [x | (x,i) <- zip xs [0..], not ( elem x (take i xs))]


--6) El producto escalar de dos listas de enteros de igual longitud
--es la suma de los productos de los elementos sucesivos (misma
--posición) de ambas listas.  Definir una función 'scalarProduct' que
--devuelva el producto escalar de dos listas.
--Sugerencia: Usar las funciones 'zip' y 'sum'. 
prod' [] [] = []
prod' (x:xs) (y:ys) = x*y : prod' xs ys
scalarProduct xs ys = sum (prod' xs ys)


--7) Sin usar funciones definidas en el
--preludio, defina recursivamente las siguientes funciones y
--determine su tipo más general:

--a) 'suma', que suma todos los elementos de una lista de números
suma:: Num a => [a]->a
suma [] = 0
suma (x:xs) = x + suma xs

--b) 'alguno', que devuelve True si algún elemento de una
--lista de valores booleanos es True, y False en caso
--contrario
alguno:: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = if x == True then True else alguno xs

--c) 'todos', que devuelve True si todos los elementos de
--una lista de valores booleanos son True, y False en caso
--contrario
todos:: [Bool]->Bool
todos [] = True
todos (x:xs) = if x==False then False else todos xs

--d) 'codes', que dada una lista de caracteres, devuelve la
--lista de sus ordinales
findOrd c l (x:xs) = if c == x then l else findOrd c (l+1) xs

codes [] = []
codes (x:xs) = findOrd x 0 ['a'..'z'] : codes xs

--e) 'restos', que calcula la lista de los restos de la
--división de los elementos de una lista de números dada por otro
--número dado
restos [] _ = []
restos (x:xs) n = (x `mod` n) : (restos xs n)

--f) 'cuadrados', que dada una lista de números, devuelva la
--lista de sus cuadrados
cuadrados xs = [x*x|x<-xs]

--g) 'longitudes', que dada una lista de listas, devuelve la
--lista de sus longitudes
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

longitudes xs = [longitud x | x <- xs]

--h) 'orden', que dada una lista de pares de números, devuelve
--la lista de aquellos pares en los que la primera componente es
--menor que el triple de la segunda
primero (x,_) = x
segundo (_,y) = y
orden xs = [x|x<-xs, (primero x) < 3*(segundo x)]

--i) 'pares', que dada una lista de enteros, devuelve la lista
--de los elementos pares
pares [] = []
pares xs = [x|x<-xs,x `mod` 2 == 0]

--j) 'letras', que dada una lista de caracteres, devuelve la
--lista de aquellos que son letras (minúsculas o mayúsculas)
letras xs = [x|x<-xs, (x >= 'a' && x<= 'z') || (x >= 'A' && x<= 'Z')]

--k) 'masDe', que dada una lista de listas 'xss' y un
--número 'n', devuelve la lista de aquellas listas de 'xss'
--con longitud mayor que 'n'
masDe [] _ = []
masDe (x:xs) n = if (longitud x) > n then x:masDe xs n else masDe xs n
{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}
--Input: foldr (+) 5 [1,2,3,4]
--Output: 15

--Input: map abs [-1,-3,4,-12]
--Output: [1,3,4,12]

--Input: filter odd [3,6,7,9,12,14]
--Output: [3,7,9]

--a) 'suma', que suma todos los elementos de una lista de números
suma' (x:xs) = foldr (+) x xs

--b) 'alguno', que devuelve True si algún elemento de una
--lista de valores booleanos es True, y False en caso
--contrario
alguno' (x:xs) = foldr (||) x xs

--c) 'todos', que devuelve True si todos los elementos de
--una lista de valores booleanos son True, y False en caso
--contrario
todos' (x:xs) = foldr (&&) x xs

--d) 'codes', que dada una lista de caracteres, devuelve la
--lista de sus ordinales
findOrd' l (x:xs) c = if c == x then l else findOrd c (l+1) xs
codes' xs = map (findOrd' 0 ['a'..'z']) xs

--e) 'restos', que calcula la lista de los restos de la
--división de los elementos de una lista de números dada por otro
--número dado
restos' xs n = map ( `mod` n) xs

--f) 'cuadrados', que dada una lista de números, devuelva la
--lista de sus cuadrados
cuadrados' xs = map (^2) xs

--g) 'longitudes', que dada una lista de listas, devuelve la
--lista de sus longitudes
longitudes' xs = map (longitud) xs

--h) 'orden', que dada una lista de pares de números, devuelve
--la lista de aquellos pares en los que la primera componente es
--menor que el triple de la segunda
par' (x,y) = if x < 3*y then True else False
orden' xs = filter (par') xs

--i) 'pares', que dada una lista de enteros, devuelve la lista
--de los elementos pares
pares' xs = filter (even) xs

--j) 'letras', que dada una lista de caracteres, devuelve la
--lista de aquellos que son letras (minúsculas o mayúsculas)
letra' c | c>='a' && c<='z' = True
         | c>='A' && c<='Z' = True
         | otherwise = False
letras' xs = filter (letra') xs

--k) 'masDe', que dada una lista de listas 'xss' y un
--número 'n', devuelve la lista de aquellas listas de 'xss'
--con longitud mayor que 'n'
longN n (xs) = if (longitud xs) > n then True else False
masDe' xss n = filter (longN n) xss
