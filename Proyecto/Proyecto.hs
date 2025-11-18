module Proyecto where

import Aux

{-
 - calcularFrecuencias: Calcula la frecuencia de cada carácter en una cadena
 - Ordena los caracteres, agrupa los iguales y cuenta sus repeticiones
 - Retorna lista ordenada por frecuencia DESCENDENTE
 -}

calcularFrecuencias :: String -> [(Char, Int)]
calcularFrecuencias texto = ordenarPorFrecuencia frecuencias
  where
    textoOrdenado = ordenarString texto
    grupos = agruparConsecutivos textoOrdenado
    frecuencias = contarGrupos grupos

{-
 - ordenarString: Ordena una cadena de caracteres
 -}
ordenarString :: String -> String
ordenarString [] = []
ordenarString (pivote:xs) = 
  concatenar (concatenar menores [pivote]) mayores
  where
    menores = ordenarString (filtrarMenoresChar pivote xs)
    mayores = ordenarString (filtrarMayoresChar pivote xs)

{-
 - filtrarMenoresChar: Filtra caracteres menores o iguales al pivote
 -}
filtrarMenoresChar :: Char -> String -> String
filtrarMenoresChar _ [] = []
filtrarMenoresChar pivote (x:xs)
  | x <= pivote = x : filtrarMenoresChar pivote xs
  | otherwise   = filtrarMenoresChar pivote xs

{-
 - filtrarMayoresChar: Filtra caracteres mayores al pivote
 -}
filtrarMayoresChar :: Char -> String -> String
filtrarMayoresChar _ [] = []
filtrarMayoresChar pivote (x:xs)
  | x > pivote = x : filtrarMayoresChar pivote xs
  | otherwise  = filtrarMayoresChar pivote xs

{-
 - contarGrupos: Cuenta los elementos en cada grupo y crea tuplas (elemento, cantidad)
 -}
contarGrupos :: [[a]] -> [(a, Int)]
contarGrupos [] = []
contarGrupos (grupo:grupos) = 
  case grupo of
    [] -> contarGrupos grupos
    (x:_) -> (x, longitud grupo) : contarGrupos grupos