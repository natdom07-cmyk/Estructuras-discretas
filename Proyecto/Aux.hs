module Aux where -- Creamos el modulo para las funciones auxiliares que utilizaremos en nuestro programa principal.

{-
Creamos las funciones basicas para realizar operaciones aritmeticas, es decir suma, resta, 
Apoyandome en las practicas anteiores, Aux Practica 5
-}
suma :: Int -> Int
suma n = succ n
                            --Funciones para sumar 1 o quitar 1
resta :: Int -> Int
resta n = pred n

{- Creamos las funciones basandonos en las practicas pasadas, para poder crear listas -}
longitud :: [a] -> Int
longitud [] = 0                     --Como el nombre de la funcion lo menciona esto sirve para poder calcular la longitud de la lista 
longitud (x:xs) = suma (longitud xs)

{-
 - concatenar: Une dos listas
 -}
concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys           --Esta funcion cumple con el proposito de poder concatenar dos listas 
concatenar (x:xs) ys = x : concatenar xs ys

{-
 - reversa: Invierte una lista 
 Ejemplo basicon tenemos una lista [1,2,3,4] esto pasa a --> [4,3,2,1]
 -}
reversa :: [a] -> [a]
reversa [] = []             
reversa (x:xs) = concatenar (reversa xs) [x]


{- 
Tenemos que definir la estructura de un arbol binario, esta misma la podemos sacar como se definieron los arboles binarios
en nuestra practica 6, usamos los metodos que nos proporciono el profesor (En un commit anterior bore la definicion de arbol binario)
-}
data Arbol a = Vacio | AB a (Arbol a) (Arbol a) 
    deriving (Eq, Ord, Show)

{-
## Notas de Desarrollo

### Corrección de Enfoque 
Inicialmente comence a implementar el algoritmo de Huffman clásico de la manera en la cual se vizualisa en el video de apoyo
proporcionado por el profe, y como se mostraba en la pagina de constructor de arboles de huffman, igual dada por el profe 
pero el proyecto requiere una variación de lo de huffman donde:
- Se construye de ARRIBA hacia ABAJO
- Se inserta en orden descendente de frecuencia
- El símbolo de mayor frecuencia va a la DERECHA
- Se expande siempre la hoja más a la izquierda y profunda
 -}

data NodoHuffman = Hoja Char Int | Nodo Int
    deriving (Eq, Show)

{-
 - ArbolHuffman: Tipo del árbol de Huffman usando la estructura de Arbol de la Práctica 6
 -}
type ArbolHuffman = Arbol NodoHuffman

{-
 - obtenerFrecuencia: Obtiene la frecuencia de un nodo Huffman
 -}
obtenerFrecuencia :: NodoHuffman -> Int
obtenerFrecuencia (Hoja _ freq) = freq
obtenerFrecuencia (Nodo freq) = freq
{-
 - numElementos: Cuenta el número de nodos en un árbol
 - Basado en el ejemplo de práctica de árboles binarios
 -}
numElementos :: Arbol a -> Int
numElementos Vacio = 0
numElementos (AB _ Vacio Vacio) = 1
numElementos (AB _ izq der) = 1 + numElementos izq + numElementos der

{-
 - profundidad: Calcula la profundidad de un árbol
 - Basado en el ejemplo de práctica de árboles binarios
 -}
profundidad :: Arbol a -> Int
profundidad Vacio = 0
profundidad (AB _ Vacio Vacio) = 1
profundidad (AB _ izq der) = 1 + max (profundidad izq) (profundidad der)

{-
 - agruparConsecutivos: Agrupa elementos consecutivos iguales en sublistas
 - Necesario para calcular frecuencias de caracteres
 -}
agruparConsecutivos :: Eq a => [a] -> [[a]]
agruparConsecutivos [] = []
agruparConsecutivos (x:xs) = (x : iguales) : agruparConsecutivos diferentes
  where
    iguales = tomarMientras (== x) xs
    diferentes = descartarMientras (== x) xs

{-
 - tomarMientras: Toma elementos mientras cumplan una condición
 -}
tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = []
tomarMientras pred (x:xs) | pred x    = x : tomarMientras pred xs | otherwise = []

{-
 - descartarMientras: Descarta elementos mientras cumplan una condición
 -}
descartarMientras :: (a -> Bool) -> [a] -> [a]
descartarMientras _ [] = []
descartarMientras pred (x:xs) | pred x    = descartarMientras pred xs | otherwise = x:xs

{-
 - buscarEnTabla: Busca un elemento en una lista de tuplas (tabla de códigos)
 - Esta funcion aux, nos ayudara a visualizar y analizar si un elemento si esta en la tabla o no.
 -   tabla = [('A', "1"), ('B', "01"), ('C', "001")]
 -   buscarEnTabla 'A' tabla  -- Just "1"
 -   buscarEnTabla 'X' tabla  -- Nothing
 -   buscarEnTabla 'B' tabla  -- Just "01"
 -}
buscarEnTabla :: Eq a => a -> [(a, b)] -> Maybe b
buscarEnTabla _ [] = Nothing
buscarEnTabla clave ((k, v):xs) | clave == k = Just v | otherwise  = buscarEnTabla clave xs

{-
 - ordenarPorFrecuencia: Ordena una lista de tuplas (Char, Int) por frecuencia DESCENDENTE ya que la variacion 
    de huffman pide que sea asi
 -   ordenarPorFrecuencia [('a', 3), ('b', 5), ('c', 1)]
 -   Resultado: [('b', 5), ('a', 3), ('c', 1)]
 -}
ordenarPorFrecuencia :: [(Char, Int)] -> [(Char, Int)]
ordenarPorFrecuencia [] = []
ordenarPorFrecuencia (pivote:xs) = 
  concatenar (concatenar mayores [pivote]) menores
  where
    mayores = ordenarPorFrecuencia (filtrarMayores pivote xs)
    menores = ordenarPorFrecuencia (filtrarMenores pivote xs)

{-
 - filtrarMayores: Filtra elementos con frecuencia mayor al pivote
 - CORRECCIÓN: Se usa snd para extraer la frecuencia y mantener el pivote original
 - El error anterior era crear un nuevo pivote (c, freqPivote) en cada recursión
 - usando el carácter del elemento actual, cuando debía mantenerse el pivote original
 - Ahora pasamos el pivote completo y usamos snd para comparar frecuencias
 -}
filtrarMayores :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
filtrarMayores pivote lista =
  case lista of
    [] -> []
    (x:xs) ->
      if snd x > snd pivote
        then x : filtrarMayores pivote xs
        else filtrarMayores pivote xs

{-
 - filtrarMenores: Filtra elementos con frecuencia menor o igual al pivote
 - CORRECCIÓN: Se usa snd para extraer la frecuencia y mantener el pivote original
 - El error anterior era crear un nuevo pivote (c, freqPivote) en cada recursión
 - usando el carácter del elemento actual, cuando debía mantenerse el pivote original
 - Ahora pasamos el pivote completo y usamos snd para comparar frecuencias
 -}
filtrarMenores :: (Char, Int) -> [(Char, Int)] -> [(Char, Int)]
filtrarMenores pivote lista =
  case lista of
    [] -> []
    (x:xs) ->
      if snd x <= snd pivote
        then x : filtrarMenores pivote xs
        else filtrarMenores pivote xs
