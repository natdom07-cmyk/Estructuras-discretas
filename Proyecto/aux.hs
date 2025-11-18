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
en nuestra practica 6, usamos los metodos que nos proporciono el profesor
data Arbol a = Vacio | AB a (Arbol a) (Arbol a) 
  deriving (Eq, Ord, Show)
-}

{-
 - Idea para definir un arbol binario de huffman, crear una funcion en la cual podamos crear Nodos, de huffman
 Ademas de poder crear las hojas las cuales por definicion de como lo estamos viendo en el problema, tiene que tener la frecuencia
 es decir la frecuencia en la cual se repiten los caracteres mas usados del texto a dar.
 Ademas de tener la frecuencia tiene que tener el caracter. 
 -En este caso de esa manera podriamos definir la hoja, aunque falta definir lo que es un nodo en este caso. 


Posible estructura de lo anterior 
data NodoHuffman = Hoja Char Int | Nodo Int  --Esto lo ponemos como una manera de poder definir lo de la idea
    deriving(Eq, Show) -- Esto lo añadimos para poder imprimir y hacer una comparacion


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