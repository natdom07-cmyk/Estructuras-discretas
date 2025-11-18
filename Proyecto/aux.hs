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