--5. Definir las siguientes funciones sobre arboles binarios de busqueda (bst):
--a. maximun :: BST a -> a, que calcula el mximo valor en un bst.
--b. checkBST :: BST a -> Bool , que chequea si un arbol binario es un bst.
data BST a = Hoja | Nodo (BST a) a (BST a) deriving Show

maximun :: BST a -> a
maximun (Nodo _ b Hoja) = b
maximun (Nodo l b r) = maximun r

minimun :: BST a -> a
minimun (Nodo Hoja b _) = b
minimun (Nodo l b r) = minimun l

--checkBST BST a -> Bool
checkBST (Hoja) = True
checkBST (Nodo Hoja b Hoja) = True
checkBST (Nodo l b r) | b < minimun (Nodo l b r) = False
                      | b >= maximun (Nodo l b r) = False
                      | otherwise = checkBST l && checkBST r

--6. Si un arbol binario es dado como un nodo con dos subrboles identicos 
--se puede aplicar la tecnica sharing, para que los subarboles sean 
--representados por el mismo arbol. Definir las siguientes funciones de 
--manera que se puedan compartir la mayor cantidad posible de elementos de los
--arboles creados:
--a) completo :: a -> Int -> Tree a, tal que dado un valor x de tipo a y un 
--entero d, crea un arbol binario completo de altura d con el valor x en 
--cada nodo.
--b) balanceado :: a -> Int -> Tree a, tal que dado un valor x de tipo a y 
--un entero n, crea un arbol binario balanceado de tamano n, con el valor x 
--en cada nodo.

completo :: a -> Int -> BST a
completo x 1 = (Nodo Hoja x Hoja)
completo x d =  let 
                    nodo = completo x (d-1)
                in
                    (Nodo nodo x nodo)

balanceado :: a -> Int -> BST a
balanceado x 0 = Hoja
balanceado x 1 = (Nodo Hoja x Hoja)
balanceado x n  | (odd n) = let
                                num = n - 1
                                nDiv = (num `div` 2)
                                t = balanceado x nDiv
                            in
                                (Nodo t x t)
                | otherwise = let
                                num = n - 1
                                nDiv = (num `div` 2)
                                l = balanceado x (nDiv +1)
                                r = balanceado x nDiv
                              in
                                (Nodo l x r)

--8. La definicion de member dada en teoria (la cual determina si un elemento
--estaba en un bst), realiza en el peor caso 2 * d comparaciones, donde d es 
--la altura del arbol. Dar una definicion de member que realice a lo sumo d + 1
--comparaciones. Para ello definir member en terminos de una funcion auxiliar
--que tenga como parametro el elemento candidato, el cual puede ser igual al
--elemento que se desea buscar (por ejemplo, el ultimo elemento para el cual 
--la comparacion de a <= b retorno True) y que chequee que los elementos son 
--iguales solo cuando llega a una hoja del arbol.

member' x c Hoja = if x == c then True else False
member' x c (Nodo l b r) = if x <= b then member' x b l else member' x c r
member :: (Ord a) => a -> BST a -> Bool
member x (Nodo l b r) = member' x b (Nodo l b r)


