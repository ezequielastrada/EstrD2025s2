-- ****** TRABAJO PRÁCTICO N°1 ****** ESTRUD 2025 S1

sucesor :: Int -> Int
-- Dado un número devuelve su sucesor
sucesor x = x + 1

sumar :: Int -> Int -> Int
-- Dados dos números devuelve su suma utilizando la operación +.
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
-- Dado dos números, devuelve un par donde la primera componente es la división del
-- primero por el segundo, y la segunda componente es el resto de dicha división. Nota:
-- para obtener el resto de la división utilizar la función mod :: Int -> Int -> Int,
-- provista por Haskell
divisionYResto x y = (div x y, mod x y)

maxDelPar :: (Int,Int) -> Int
-- Dado un par de números devuelve el mayor de estos
maxDelPar (x,y) = if x > y then x else y

data Dir = Norte | Sur | Este | Oeste deriving Show

opuesto :: Dir -> Dir
-- Dada una dirección devuelve su opuesta
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
-- Dadas dos direcciones, indica si son la misma. Nota: utilizar pattern matching y no ==.
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir -> Dir
--Dada una dirección devuelve su siguiente, en sentido horario, y sup oniendo que no existe
--la siguiente dirección a Oeste. ¾Posee una precondición esta función? ¾Es una función
--total o parcial? Por qué?
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "No hay siguiente a oeste"

{-
Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Sup ongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones:
-}

data DiaDeSemana = Domingo | Lunes | Martes | Miercoles | Jueves | Viernes | Sabado deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
-- Devuelve un par donde la primera componente es el primer día de la semana, y la
-- segunda componente es el último día de la semana. Considerar definir subtareas útiles
-- que puedan servir después.
primeroYUltimoDia = (primerDiaSemana, ultimoDiaSemana)

primerDiaSemana :: DiaDeSemana
primerDiaSemana = Lunes

ultimoDiaSemana :: DiaDeSemana
ultimoDiaSemana = Domingo

empiezaConM :: DiaDeSemana -> Bool
-- Dado un día de la semana indica si comienza con la letra M.
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
-- Dado dos días de semana, indica si el primero viene después que el segundo. Analizar
-- la calidad de la solución respecto de la cantidad de casos analizados (entre los casos
-- analizados en esta y cualquier subtarea, deberían ser no más de 9 casos).
vieneDespues x y = posicionDeDiaEnSemana x > posicionDeDiaEnSemana y

posicionDeDiaEnSemana :: DiaDeSemana -> Int
posicionDeDiaEnSemana Lunes = 0
posicionDeDiaEnSemana Martes = 1
posicionDeDiaEnSemana Miercoles = 2
posicionDeDiaEnSemana Jueves = 3
posicionDeDiaEnSemana Viernes = 4
posicionDeDiaEnSemana Sabado = 5
posicionDeDiaEnSemana Domingo = 6

estaEnElMedio :: DiaDeSemana -> Bool
-- Dado un día de la semana indica si no es ni el primer ni el ultimo dia.
estaEnElMedio d = estaDespuesDePrimerDia d && estaAntesDeUltimoDiaSemana d

estaDespuesDePrimerDia :: DiaDeSemana -> Bool
estaDespuesDePrimerDia d = (posicionDeDiaEnSemana d) > (posicionDeDiaEnSemana primerDiaSemana)

estaAntesDeUltimoDiaSemana :: DiaDeSemana -> Bool
estaAntesDeUltimoDiaSemana d = (posicionDeDiaEnSemana d) < (posicionDeDiaEnSemana ultimoDiaSemana)

negar :: Bool -> Bool
-- Dado un booleano, si es True devuelve False, y si es False devuelve True.
-- En Haskell ya está definida como not.
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
-- Dados dos booleanos, si el primero es True y el segundo es False, devuelve False, sino
-- devuelve True.
-- Esta función NO deb e realizar doble pattern matching.
-- Nota: no viene implementada en Haskell.
implica True b = b
implica _    _ = True

yTambien :: Bool -> Bool -> Bool
-- Dados dos booleanos si ambos son True devuelve True, sino devuelve False.
-- Esta función NO deb e realizar doble pattern matching.
-- En Haskell ya está definida como &&
yTambien True b = b
yTambien _ _ = False

oBien :: Bool -> Bool -> Bool
-- Dados dos booleanos si alguno de ellos es True devuelve True, sino devuelve False.
-- Esta función NO deb e realizar doble pattern matching.
-- En Haskell ya está definida como ||.
oBien True _ = True
oBien _ True = True
oBien _ _ = False


--Definir el tipo de dato Persona, como un nombre y la edad de la persona.
data Persona = P String Int deriving Show

nombre :: Persona -> String
-- Devuelve el nombre de una persona
nombre (P n e) = n

edad :: Persona -> Int
-- Devuelve la edad de una persona
edad (P n e) = e

crecer :: Persona -> Persona
-- Aumenta en uno la edad de la persona.
crecer (P n e)  = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
-- Dados un nombre y una persona, devuelve una persona con la edad de la persona y el nuevo nombre.
cambioDeNombre s (P n e) = P s e

esMayorQueLaOtra :: Persona -> Persona -> Bool
-- Dadas dos personas indica si la primera es mayor que la segunda.
esMayorQueLaOtra p1 p2 = edad p1 > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
-- Dadas dos personas devuelve a la persona que sea mayor.
laQueEsMayor p r = if esMayorQueLaOtra p r then p else r

-- Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
-- porcenta je de energía; y Entrenador, como un nombre y dos Pokémon. 

data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pok TipoDePokemon Int deriving Show
data Entrenador = E String Pokemon Pokemon deriving Show

superaA :: Pokemon -> Pokemon -> Bool
-- Dados dos Pokémon indica si el primero, en base al tipo, es superior al segundo. Agua
-- supera a fuego, fuego a planta y planta a agua. Y cualquier otro caso es falso.
superaA p1 p2 = tipoSupera (tipoPokemon p1) (tipoPokemon p2)

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (Pok t _) = t

tipoSupera :: TipoDePokemon -> TipoDePokemon -> Bool
tipoSupera Agua Fuego = True
tipoSupera Fuego Planta = True
tipoSupera Planta Agua = True
tipoSupera _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantidadDePokemonDe t (E n p1 p2) = unoSiEsTipo (tipoPokemon p1) t + unoSiEsTipo (tipoPokemon p2) t

unoSiEsTipo :: TipoDePokemon -> TipoDePokemon -> Int
unoSiEsTipo Fuego Fuego =  1
unoSiEsTipo Agua Agua =  1
unoSiEsTipo Planta Planta =  1
unoSiEsTipo _ _ = 0

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
-- Dado un par de entrenadores, devuelve a sus Pokémon en una lista.
juntarPokemon (e1,e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (E _ p1 p2) = p1:p2:[]

loMismo :: a -> a
-- Dado un elemento de algún tip o devuelve ese mismo elemento.
loMismo a = a

siempreSiete :: a -> Int
-- Dado un elemento de algún tipo devuelve el número 7.
siempreSiete _ = 7

swap :: (a,b) -> (b, a)
-- Dadas una tupla, invierte sus componentes.
swap (a,b) = (b,a)

estaVacia :: [a] -> Bool
-- Dada una lista de elementos, si es vacía devuelve True, sino devuelve False.
-- Definida en Haskell como null.
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
-- Dada una lista devuelve su primer elemento.
-- Definida en Haskell como head.
-- Nota: tener en cuenta que el constructor de listas es :
elPrimero (x:_) = x
elPrimero _ = error "La lista está vacia"

sinElPrimero :: [a] -> [a]
-- Dada una lista devuelve esa lista menos el primer elemento.
-- Definida en Haskell como tail.
-- Nota: tener en cuenta que el constructor de listas es :
sinElPrimero (_:x) = x
sinElPrimero _ = error "La lista está vacia"

splitHead :: [a] -> (a, [a])
-- Dada una lista devuelve un par, donde la primera comp onente es el primer elemento de la
-- lista, y la segunda comp onente es esa lista p ero sin el primero.
-- Nota: tener en cuenta que el constructor de listas es :
splitHead (x:xs) = (x,xs)
splitHead _ = error "La lista está vacia"
