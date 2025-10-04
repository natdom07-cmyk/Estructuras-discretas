module Aux where

{-
 - reversa: Invierte una lista
-}
 
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

{-
 - decrementar: Resta 1 a un número
-}
 
decrementar :: Int -> Int
decrementar n = pred n

{-
 - incrementar: Suma 1 a un número
-}

incrementar :: Int -> Int
incrementar n = succ n

{-
 - ultimo: Obtiene el último elemento de una lista
-}

ultimo :: [a] -> a
ultimo [x] = x
ultimo (x:xs) = ultimo xs

{-
 - sinUltimo: Devuelve la lista sin el último elemento
-}

sinUltimo :: [a] -> [a]
sinUltimo [x] = []
sinUltimo (x:xs) = x : sinUltimo xs

{-
 - esPar: Verifica si un número es par
-}

esPar :: Int -> Bool
esPar 0 = True
esPar 1 = False
esPar n
  | n < 0 = esPar (negar n)
  | otherwise = esPar (decrementar (decrementar n))

{-
 - negar: Cambia el signo de un número
-}

negar :: Int -> Int
negar n = 0 - n

{-
 - mitad: Divide un número entre 2
-}
mitad :: Int -> Int
mitad 0 = 0
mitad n
  | n < 0 = negar (mitad (negar n))
  | otherwise = mitadPositiva n 0

{-
 - mitadPositiva: Función auxiliar para calcular la mitad
 - Cuenta cuántas veces se puede restar 2
-}

mitadPositiva :: Int -> Int -> Int
mitadPositiva 0 contador = contador
mitadPositiva 1 contador = contador
mitadPositiva n contador = mitadPositiva (decrementar (decrementar n)) (incrementar contador)

{-
 - triplicar: Multiplica un número por 3
-}

triplicar :: Int -> Int
triplicar 0 = 0
triplicar n
  | n < 0 = negar (triplicar (negar n))
  | otherwise = sumar n (sumar n n)

{-
 - sumar: Suma dos números
-}

sumar :: Int -> Int -> Int
sumar 0 b = b
sumar a b
  | a < 0 = decrementar (sumar (incrementar a) b)
  | otherwise = incrementar (sumar (decrementar a) b)
  