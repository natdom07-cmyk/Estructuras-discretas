module Practica5 where

import Data.Char
import Aux

{-
 - hollerBack: Convierte una cadena a mayúsculas
 -}
 
hollerBack :: String -> String
hollerBack [] = []
hollerBack (x:xs) = toUpper x : hollerBack xs

{-
 - palindromo: Verifica si una lista es palíndromo
 -}
 
palindromo :: Eq a => [a] -> Bool
palindromo xs = xs == reversa xs

{-
 - replica: Repite un elemento n veces
 -}
 
replica :: a -> Int -> [a]
replica _ 0 = []
replica x n = x : replica x (decrementar n)

{-
 - recuperaElemento: Obtiene el elemento en un índice específico
 -}
 
recuperaElemento :: [a] -> Int -> a
recuperaElemento [] _ = error "Lista vacia"
recuperaElemento (x:xs) 0 = x
recuperaElemento (x:xs) n
  | n < 0 = error "Indice negativo"
  | otherwise = recuperaElemento xs (decrementar n)

{-
 - rota: Rota una lista n posiciones
 -}
 
rota :: [a] -> Int -> [a]
rota [] _ = []
rota xs 0 = xs
rota xs n
  | n > 0 = rota (rotaUna xs) (decrementar n)
  | otherwise = rota (rotaUnaAtras xs) (incrementar n)

{-
 - rotaUna: Rota una lista una posición a la derecha
 -}
 
rotaUna :: [a] -> [a]
rotaUna [] = []
rotaUna [x] = [x]
rotaUna (x:xs) = xs ++ [x]

{-
 - rotaUnaAtras: Rota una lista una posición a la izquierda
 -}
 
rotaUnaAtras :: [a] -> [a]
rotaUnaAtras [] = []
rotaUnaAtras xs = ultimo xs : sinUltimo xs

{-
 - extranio: Genera la secuencia del algoritmo extraño
 -}
 
extranio :: Int -> [Int]
extranio 1 = [1]
extranio n
  | esPar n = n : extranio (mitad n)
  | otherwise = n : extranio (incrementar (triplicar n))