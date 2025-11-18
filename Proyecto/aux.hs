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
-}

data Arbol a = Vacio | AB a (Arbol a) (Arbol a) 
  deriving (Eq, Ord, Show)

{-
 - Idea para definir un arbol binario de huffman, crear una funcion en la cual podamos crear Nodos, de huffman
 Ademas de poder crear las hojas las cuales por definicion de como lo estamos viendo en el problema, tiene que tener la frecuencia
 es decir la frecuencia en la cual se repiten los caracteres mas usados del texto a dar.
 Ademas de tener la frecuencia tiene que tener el caracter. 
 -En este caso de esa manera podriamos definir la hoja, aunque falta definir lo que es un nodo en este caso. 


Posible estructura de lo anterior 
data NodoHuffman = Hoja Char Int | Nodo Int  --Esto lo ponemos como una manera de poder definir lo de la idea
    deriving(Eq, Show) -- Esto lo añadimos para poder imprimir y hacer una comparacion

 -}

 