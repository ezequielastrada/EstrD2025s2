sumatoria :: [Int] -> Int
-- Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
--Dada una lista de elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
-- de elementos que posee.
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
-- Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.
sucesores [] = []
sucesores (x:xs) = (x+1) : sucesores xs

conjuncion :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si todos sus elementos son True.
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs
-- aclaración: lista vacia devuelve True

disyuncion :: [Bool] -> Bool
-- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs
-- aclaración: lista vacia devuelve False

aplanar :: [[a]] -> [a]
-- Dada una lista de listas, devuelve una única lista con todos sus elementos.
aplanar [] = []
aplanar (xs:xss) = xs ++ aplanar xss

pertenece :: Eq a => a -> [a] -> Bool
-- Dados un elemento e y una lista xs devuelve True si existe un elemento en xs que sea igual
pertenece a (x:xs) = a == x || pertenece a xs

apariciones :: Eq a => a -> [a] -> Int
-- Dados un elemento e y una lista xs cuenta la cantidad de apariciones de e en xs.
apariciones _ [] = 0
apariciones e (x:xs) = unoSiSonIguales(e == x) + apariciones e xs

unoSiSonIguales :: Bool -> Int
unoSiSonIguales True = 1
unoSiSonIguales False = 0

losMenoresA :: Int -> [Int] -> [Int]
-- Dados un número n y una lista xs, devuelve todos los elementos de xs que son menores a n.
losMenoresA _ [] = []
losMenoresA n (i:is) = 
    if i < n 
        then i : losMenoresA n is 
        else losMenoresA n is

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
-- Dados un número n y una lista de listas, devuelve la lista de aquellas listas que tienen más
-- de n elementos
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (xs:xss) = 
    if longitud xs > n
        then xs : lasDeLongitudMayorA n xss
        else lasDeLongitudMayorA n xss

agregarAlFinal :: [a] -> a -> [a]
-- Dados una lista y un elemento, devuelve una lista con ese elemento agregado al final de la lista.
agregarAlFinal [] e = [e]
agregarAlFinal (x:xs) e = x : agregarAlFinal xs e

agregar :: [a] -> [a] -> [a]
-- Dadas dos listas devuelve la lista con todos los elementos de la primera lista y todos los
-- elementos de la segunda a continuación. Definida en Haskell como (++).
agregar [] ys = []
agregar (x:xs) ys = x : agregar xs ys


reversa :: [a] -> [a]
--Dada una lista devuelve la lista con los mismos elementos de atrás para adelante. Denida
--en Haskell como reverse.
reversa [] = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la p osición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos _ _ = error "falta implementar"



elMinimo :: Ord a => [a] -> a
-- Dada una lista devuelve el mínimo
elMinimo _ = error "falta implementar"

--Defina las siguientes funciones utilizando recursión sobre números enteros, salvo que se indique
--lo contrario:

factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número y to dos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial n = 
    if n == 0
        then 1
        else n * factorial (n-1)

cuentaRegresiva :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
-- n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva n = 
    if n < 1
        then []
        else n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
-- Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir 0 _ = []
repetir n a = a : repetir (n-1) a

losPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
--Si la lista es vacía, devuelve una lista vacía.
losPrimeros _ [] = []
losPrimeros 0 _ = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros 0 xs = xs
sinLosPrimeros n [] = []
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

--Registros
-- Definir el tipo de dato Persona, como un nombre y la edad de la p ersona. Realizar las
-- siguientes funciones:
data Persona = P String Int deriving Show -- nombre y edad

mayoresA :: Int -> [Persona] -> [Persona]
-- Dados una edad y una lista de personas devuelve a las p ersonas mayores a esa edad.
mayoresA _ [] = []
mayoresA n (p:ps) = 
    if edadPersona p > n
        then p : mayoresA n ps
        else mayoresA n ps

edadPersona :: Persona -> Int
edadPersona (P _ e) = e


promedioEdad :: [Persona] -> Int
-- Dada una lista de p ersonas devuelve el promedio de edad entre esas personas. Precon-
-- dición : la lista al menos p osee una p ersona.
promedioEdad ps = div (sumarTodasEdades ps) (longitud ps)

sumarTodasEdades :: [Persona] -> Int
sumarTodasEdades [] = 0
sumarTodasEdades (p:ps) = edadPersona p + sumarTodasEdades ps


elMasViejo :: [Persona] -> Persona
-- Dada una lista de p ersonas devuelve la p ersona más vieja de la lista. Precondición : la
-- lista al menos p osee una p ersona.
elMasViejo (p:[]) = p
elMasViejo (p:ps) = laPersonaMasVieja p (elMasViejo ps)

laPersonaMasVieja :: Persona -> Persona -> Persona
laPersonaMasVieja p1 p2 = 
    if edadPersona p1 > edadPersona p2
        then p1
        else p2

--Modificaremos la representación de Entreador y Pokemon de la práctica anterior de la si-
--guiente manera:

data TipoDePokemon = Agua | Fuego | Planta
data Pokemon = ConsPokemon TipoDePokemon Int
data Entrenador = ConsEntrenador String [Pokemon]

--Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
--Definir en base a esa representación las siguientes funciones:

cantPokemon :: Entrenador -> Int
-- Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon (ConsEntrenador _ p) = longitud p


cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe t (ConsEntrenador _ ps) = cantTipoPokemon t ps

cantTipoPokemon :: TipoDePokemon -> [Pokemon] -> Int
cantTipoPokemon _ [] = 0
cantTipoPokemon t (p:ps) =
    if esMismoTipo t (tipoPokemon p)
        then 1 + cantTipoPokemon t ps
        else cantTipoPokemon t ps

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (ConsPokemon t _) = t

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua Agua = True
esMismoTipo Fuego Fuego = True
esMismoTipo Planta Planta = True
esMismoTipo _ _ = False

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
-- Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían
-- a los Pokemon del segundo entrenador.
cuantosDeTipo_De_LeGananATodosLosDe_ t e1 e2 =
    cuantosPokemones_LeGananATodos t (pokemonesEntrenador e1) (pokemonesEntrenador e2)

pokemonesEntrenador :: Entrenador -> [Pokemon]
pokemonesEntrenador (ConsEntrenador _ pks) = pks

cuantosPokemones_LeGananATodos :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantosPokemones_LeGananATodos t [] _ = 0
cuantosPokemones_LeGananATodos t (x:xs) pks =
    if esMismoTipo t (tipoPokemon x)
        then unoSiLeGanaAtodos x pks + cuantosPokemones_LeGananATodos t xs pks
        else cuantosPokemones_LeGananATodos t xs pks

unoSiLeGanaAtodos :: Pokemon -> [Pokemon] -> Int
unoSiLeGanaAtodos p [] = 0
unoSiLeGanaAtodos p pks = 
    if leGanaATodos p pks
        then 1
        else 0

leGanaATodos :: Pokemon -> [Pokemon] -> Bool
leGanaATodos _ [] = True
leGanaATodos p (pk:pks) = leGana (tipoPokemon p) (tipoPokemon pk) && leGanaATodos p pks

leGana :: TipoDePokemon -> TipoDePokemon -> Bool
leGana Agua Fuego = True
leGana Fuego Planta = True
leGana Planta Agua = True
leGana _ _ = False

esMaestroPokemon :: Entrenador -> Bool
-- Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.
esMaestroPokemon (ConsEntrenador _ ps) = tieneTodosLosTipos ps todosLosTipos

todosLosTipos = [Agua,Fuego,Planta]

tieneTodosLosTipos :: [Pokemon] -> [TipoDePokemon]-> Bool
tieneTodosLosTipos _ [] = True
tieneTodosLosTipos ps (t:ts) = hayDeTipo t ps && tieneTodosLosTipos ps ts 

hayDeTipo ::  TipoDePokemon -> [Pokemon] -> Bool
hayDeTipo _ [] = False
hayDeTipo t (po:ps) = esMismoTipo t (tipoPokemon po) || hayDeTipo t ps



data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]

proyectos :: Empresa -> [Proyecto]
-- Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
proyectos (ConsEmpresa roles) = proyectosDeRoles roles

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (r:rs) = agregarProyectoSiNoEsta (proyectoDeRol r) (proyectosDeRoles rs)

agregarProyectoSiNoEsta :: Proyecto -> [Proyecto] -> [Proyecto]
agregarProyectoSiNoEsta p ps = if perteneceProyecto p ps then ps else p : ps

perteneceProyecto :: Proyecto -> [Proyecto] -> Bool
perteneceProyecto p [] = False
perteneceProyecto pr (p:ps) = esMismoProyecto pr p || perteneceProyecto pr ps

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto n) = n

esMismoProyecto :: Proyecto -> Proyecto -> Bool
esMismoProyecto p1 p2 = nombreProyecto p1 == nombreProyecto p2

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol (Developer _ p) = p
proyectoDeRol (Management _ p) = p

losDevSenior :: Empresa -> [Proyecto] -> Int
-- Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
-- además a los proyectos dados por parámetro.
losDevSenior (ConsEmpresa rs) ps = rolesDevSenior rs ps 

rolesDevSenior :: [Rol] -> [Proyecto] -> Int
rolesDevSenior [] _ = 0
rolesDevSenior (r:rs) ps = unoSiProyectoEsta (proyectoDeRolDevSenior r) ps + rolesDevSenior rs ps

proyectoDeRolDevSenior :: Rol -> [Proyecto]
proyectoDeRolDevSenior (Developer s p) = proyectoDeRolDevSenior' s p
proyectoDeRolDevSenior _ = []

proyectoDeRolDevSenior' :: Seniority -> Proyecto -> [Proyecto]
proyectoDeRolDevSenior' Senior p = [p]
proyectoDeRolDevSenior' _ _ = []

unoSiProyectoEsta :: [Proyecto] -> [Proyecto] -> Int
unoSiProyectoEsta [] _ = 0
unoSiProyectoEsta (p:ps) prs = if (perteneceProyecto p prs) then 1 else 0

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
-- Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
cantQueTrabajanEn p (ConsEmpresa r) = cantQueTrabajanEnProyecto p r

cantQueTrabajanEnProyecto :: [Proyecto] -> [Rol] -> Int
cantQueTrabajanEnProyecto _ [] = 0
cantQueTrabajanEnProyecto ps (r:rs) = unoSiTrabajaEnProyectoDeRol ps r + cantQueTrabajanEnProyecto ps rs

unoSiTrabajaEnProyectoDeRol :: [Proyecto] -> Rol -> Int
unoSiTrabajaEnProyectoDeRol [] _ = 0
unoSiTrabajaEnProyectoDeRol (p:ps) r = if sonMismoProyecto p r then 1 else unoSiTrabajaEnProyectoDeRol ps r

sonMismoProyecto :: Proyecto -> Rol -> Bool
sonMismoProyecto p r = nombreProyecto p == nombreProyecto(proyectoDeRol r)

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
-- Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
-- cantidad de personas involucradas.
asignadosPorProyecto (ConsEmpresa rs) = asignadosPorProyectoEmpresa rs

asignadosPorProyectoEmpresa :: [Rol] -> [(Proyecto, Int)]
asignadosPorProyectoEmpresa []  = []
asignadosPorProyectoEmpresa (r:rs) = agregarProyectoATupla r (asignadosPorProyectoEmpresa rs)

agregarProyectoATupla :: Rol -> [(Proyecto, Int)] -> [(Proyecto, Int)]
agregarProyectoATupla r [] = [((proyectoDeRol r),1)]
agregarProyectoATupla r ((p,i):xs) = if (esMismoProyecto (proyectoDeRol r) p) 
    then (p,i+1) : xs
    else (p,i) : agregarProyectoATupla r xs

sumarUnoATupla :: (Proyecto,Int) -> [(Proyecto,Int)]
sumarUnoATupla (p,i) = [(p,(i+1))]
