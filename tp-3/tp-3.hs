-- 1. Tipos recursivos simples
-- 1.1. Celdas con bolitas
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

nroBolitas :: Color -> Celda -> Int
-- Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
-- existe una operación sobre listas que ayude a resolver el problema
nroBolitas _ CeldaVacia = 0
nroBolitas x (Bolita color celda ) = if (esMismocolor x color)
    then 1 + nroBolitas x celda
    else nroBolitas x celda

esMismocolor :: Color -> Color -> Bool
esMismocolor Rojo Rojo = True
esMismocolor Azul Azul = True
esMismocolor _ _ = False

poner :: Color -> Celda -> Celda
-- Dado un color y una celda, agrega una bolita de dicho color a la celda
poner c CeldaVacia = Bolita c CeldaVacia
poner c (Bolita color celda) = Bolita color (poner c celda)

sacar :: Color -> Celda -> Celda
-- Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
-- Gobstones, esta función es total
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita color celda) = if esMismocolor c color
    then celda
    else sacar c celda

ponerN :: Int -> Color -> Celda -> Celda
-- Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN 0 c celda = celda

ponerN n c celda = ponerN (n-1) c (Bolita c celda)
-- pone la bolita al final, pensar como poner adelante


-- 1.2. Camino hacia el tesoro
data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

camino1 = Cofre cofre1 Fin
cofre1 = [Tesoro]
cofre2 = [Cacharro]

hayTesoro :: Camino -> Bool
-- Indica si hay un cofre con un tesoro en el camino
hayTesoro Fin = False
hayTesoro (Cofre os c) = hayTesoroEnCofre os || hayTesoro c
hayTesoro (Nada c) = hayTesoro c

hayTesoroEnCofre :: [Objeto] -> Bool
hayTesoroEnCofre [] = False
hayTesoroEnCofre (o:os) = esTesoro o || hayTesoroEnCofre os

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

pasosHastaTesoro :: Camino -> Int
-- Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
-- Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
-- Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin = error "Precondicion: tiene que haber al menos un tesoro"
pasosHastaTesoro (Cofre os c) = if hayTesoroEnCofre os then 0 else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c

hayTesoroEn :: Int -> Camino -> Bool
-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn _ Fin = False
hayTesoroEn 0 (Cofre os _) = hayTesoroEnCofre os
hayTesoroEn 0 _ = False
hayTesoroEn p (Cofre _ c) = hayTesoroEn (p-1) c
hayTesoroEn p (Nada c) = hayTesoroEn (p-1) c

{--
hayTesoroEn :: Int -> Camino -> Bool
-- Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
-- pasos es 5, indica si hay un tesoro en 5 pasos.
hayTesoroEn _ Fin = False
hayTesoroEn p (Cofre os c) = if (p == 0) then hayTesoroEnCofre os else hayTesoroEn (p-1)
hayTesoroEn p (Nada c) = if (p == 0) then False else hayTesoroEn (p-1) c
--}

alMenosNTesoros :: Int -> Camino -> Bool
-- Indica si hay al menos "n" tesoros en el camino.
alMenosNTesoros n c = n <= cantidadTesorosCamino c

cantidadTesorosCamino :: Camino -> Int
cantidadTesorosCamino Fin = 0
cantidadTesorosCamino (Cofre os c) = if (hayTesoroEnCofre os) then 1 + cantidadTesorosCamino c else cantidadTesorosCamino c
cantidadTesorosCamino (Nada c) = cantidadTesorosCamino c


-- 2. Tipos arbóreos
-- 2.1. Árboles binarios
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

arbol1 = NodeT 1 (NodeT 1 (NodeT 1 (EmptyT) (EmptyT)) (NodeT 2 (EmptyT) (EmptyT))) EmptyT

sumarT :: Tree Int -> Int
-- Dado un árb ol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT a ti td) = a + sumarT ti + sumarT td

sizeT :: Tree a -> Int
-- Dado un árb ol binario devuelve su cantidad de elementos, es decir, el tamaño del árb ol (size en inglés)
sizeT EmptyT = 0
sizeT (NodeT a ti td) = 1 + sizeT ti + sizeT td

mapDobleT :: Tree Int -> Tree Int
-- Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT a ti td) = NodeT (a*2) (mapDobleT ti) (mapDobleT td)

perteneceT :: Eq a => a -> Tree a -> Bool
-- Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
-- árbol.
perteneceT _ EmptyT = False
perteneceT e (NodeT a ti td) = e == a || perteneceT e ti || perteneceT e td

aparicionesT :: Eq a => a -> Tree a -> Int
-- Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árb ol que son
-- iguales a e.
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT a ti td) = if (e == a) 
    then 1 + aparicionesT e ti + aparicionesT e td
    else aparicionesT e ti + aparicionesT e td

leaves :: Tree a -> [a]
-- Dado un árbol devuelve los elementos que se encuentran en sus hojas.
-- NOTA: en este tipo se define como hoja a un nodo con dos hijos vacíos.  
leaves EmptyT = []
leaves (NodeT a ti td) = [a] ++ leaves ti ++ leaves td

heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
--Nota: la altura de un árbol (height en inglés), también llamada profundidad, es
--la cantidad de niveles del árbol . La altura para EmptyT es 0, y para una hoja es 1.
heightT EmptyT = 0
heightT (NodeT a ti td) = 1 + max (heightT ti) (heightT td)

mirrorT :: Tree a -> Tree a
-- Dado un árb ol devuelve el árb ol resultante de intercambiar el hijo izquierdo con
-- el derecho, en cada no do del árb ol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT a ti td) = NodeT a (mirrorT td) (mirrorT ti)

toList :: Tree a -> [a]
-- Dado un árb ol devuelve una lista que representa el resultado de recorrerlo en
-- mo do in-order.
-- Nota: En el modo in-order primero se procesan los elementos del hijo izquierdo,
-- luego la raiz y luego los elementos del hijo derecho.
toList EmptyT = []
toList (NodeT a ti td) = toList ti ++ [a] ++ toList td

levelN :: Int -> Tree a -> [a]
-- Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El
-- nivel de un nodo es la distancia que hay de la raíz hasta él. La distancia de la
-- raiz a sí misma es 0, y la distancia de la raiz a uno de sus hijos es 1.
-- Nota: El primer nivel de un árbol (su raíz) es 0.
levelN _ EmptyT = []
levelN 0 (NodeT a _ _) = [a]
levelN n (NodeT _ ti td) = if (n > 0) 
    then levelN (n-1) ti ++ levelN (n-1) td 
    else []

listPerLevel :: Tree a -> [[a]]
-- Dado un árbol devuelve una lista de listas en la que cada elemento representa
-- un nivel de dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT a ti td) = [a] : combinarNiveles (listPerLevel ti) (listPerLevel td)

combinarNiveles :: [[a]] -> [[a]] -> [[a]]
mercombinarNivelesgeLevels [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x ++ y) : combinarNiveles xs ys

ramaMasLarga :: Tree a -> [a]
-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT a ti td) = if (heightT ti > heightT td)
  then a : ramaMasLarga ti
  else a : ramaMasLarga td

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) = [x] : consAll x (todosLosCaminos t1 ++ todosLosCaminos t2)

consAll :: a -> [[a]] -> [[a]]
consAll _ [] = []
consAll y (xs:xss) = (y:xs) : consAll y xss

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

eval :: ExpA -> Int
eval (Valor n) = n
eval (Sum exp1 exp2) = eval exp1 + eval exp2
eval (Prod exp1 exp2) = eval exp1 * eval exp2
eval (Neg exp) = - eval exp

simplificar :: ExpA -> ExpA
simplificar (Sum e1 e2)  = simplificarSuma (simplificar e1) (simplificar e2)
simplificar (Prod e1 e2) = simplificarProd (simplificar e1) (simplificar e2)
simplificar (Neg e)      = simplificarNeg (simplificar e)
simplificar e            = e

simplificarSuma :: ExpA -> ExpA -> ExpA
simplificarSuma (Valor 0) e = e
simplificarSuma e (Valor 0) = e
simplificarSuma e1 e2 = Sum e1 e2

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 1) e = e
simplificarProd e (Valor 1) = e
simplificarProd (Valor 0) _ = Valor 0
simplificarProd _ (Valor 0) = Valor 0
simplificarProd e1 e2 = Prod e1 e2

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Neg e) = e
simplificarNeg e = Neg e
