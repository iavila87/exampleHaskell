--9. Definir una funcion fromOrdList :: [a] -> RBT a, que cree un red black 
--tree a partir de una lista ordenada sin elementos repetidos. La funcion
--debe ser de orden O(n).
data Color = R | B deriving Show
data RBT a = E | T Color (RBT a) a (RBT a) deriving Show

makeBlack E = E
makeBlack (T _ l x r) = T B l x r

balance:: Color -> RBT a -> a -> RBT a -> RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

insert:: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack(ins x t)
    where ins x E = T R E x E
          ins x (T c l y r) | x<y = balance c (ins x l) y r
                            | x>y = balance c l y (ins x r)
                            | otherwise = T c l y r

fromOrdList:: (Ord a) => [a] -> RBT a
fromOrdList [] = E
fromOrdList (x:xs) = insert' x (fromOrdList xs)

--10. La funcion insert dada en teoria para insertar un elemento en un rbt 
--puede optimizarse elimando comparaciones innecesarias hechas por la funcion
--balance. Por ejemplo, en la definicion de la funcion ins cuando se aplica
--balance sobre el resultado de aplicar insert x sobre el subarbol
--izquierdo (l) y el subarbol derecho (r), los casos de balance para testear
--que se viola el invariante 1 en el subarbol derecho no son necesarios 
--dado que r es un rbt.
insert':: Ord a => a -> RBT a -> RBT a
insert' x t = makeBlack(ins x t)
    where ins x E = T R E x E
          ins x (T c l y r) | x<y = balanceL c (ins x l) y r
                            | x>y = balanceR c l y (ins x r)
                            | otherwise = T c l y r

balanceL B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balanceL B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balanceL c l a r = T c l a r

balanceR B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balanceR B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balanceR c l a r = T c l a r