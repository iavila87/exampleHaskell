--1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde 
--y azul como colores primarios. Cualquier otro color se expresa en 
--terminos de las proporciones de estos tres colores que es necesario combinar
--en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada
--color de manera biunıvoca, por lo que usualmente se utilizan estos valores
--como representacion de un color.
--Definir un tipo Color en este modelo y una funcion mezclar que permita
--obtener el promedio componente a componente entre dos colores.
type Color = (Int,Int,Int)
prom':: Int -> Int -> Int
prom' x y = (x+y) `div` 2
mezclar:: Color -> Color -> Color
mezclar (r1,g1,b1) (r2,g2,b2) = (prom' r1 r2, prom' g1 g2, prom' b1 b2)

--2. Consideremos un editor de lıneas simple. Supongamos que una Lınea es 
--una secuencia de caracteres c1, c2, . . . , cn junto con una posicion p,
--siendo 0 <= p <= n, llamada cursor (consideraremos al cursor a la izquierda
--de un caracter que sera borrado o insertado, es decir como el cursor de la
--mayorıa de los editores). Se requieren las siguientes operaciones sobre 
--lıneas:
--vacıa :: Lınea
--moverIzq :: Lınea → Lınea
--moverDer :: Lınea → Lınea
--moverIni :: Lınea → Lınea
--moverFin :: Lınea → Lınea
--insertar :: Char → Lınea → Lınea
--borrar :: Lınea → Lınea

--La descripcion informal es la siguiente: 
--(1) la constante vacıa denota la lınea vacıa, 
--(2) la operacion moverIzq mueve el cursor una posicion a la izquierda
--(siempre que ellos sea posible),
--(3) analogamente para moverDer ,
--(4) moverIni mueve el cursor al comienzo de la lınea, 
--(5) moverFin mueve el cursor al final de la lınea,
--(6) la operacion borrar elimina el caracterer que se encuentra a la 
--izquierda del cursor, 
--(7) insertar agrega un caracter en el lugar donde se encontraba el cursor
--y lo mueve una posici ́on a la derecha.
--Definir un tipo de datos Lınea e implementar las operaciones dadas.

type Cadena = [Char]
type Linea = (Cadena,Int)
vacia:: Linea
vacia = ([],0)

moverIzq:: Linea -> Linea
moverIzq (c,0) = (c,0)
moverIzq (c,p) = (c,p-1)

moverDer:: Linea -> Linea
moverDer ([],p) = ([],0)
moverDer (c,p) = if p == (length c) - 1 then (c,p) else (c,p+1)

moverIni:: Linea -> Linea
moverIni (c,_) = (c,0)

moverFin:: Linea -> Linea
moverFin (c,p) = if (length c)==0 then (c,0) else (c,(length c)-1)

insertar:: Char -> Linea -> Linea
ins' c xs 0 = c:xs
ins' c (x:xs) n = x: ins' c xs (n-1)
insertar c (cs,p) = (ins' c cs p,p+1)
--borrar :: Lınea → Lınea
borrar:: Linea -> Linea
borrar' cs 0 = cs
borrar' (c:cs) 1 = cs
borrar' (c:cs) n = c: borrar' cs (n-1)
borrar ([],p) = ([],0)
borrar (cs,0) = (cs,0)
borrar (cs,p) = (borrar' cs p, p-1)
