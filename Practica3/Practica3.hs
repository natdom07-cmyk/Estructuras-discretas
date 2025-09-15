-- Practica 03: Tipos de datos y listas por comprensión

--EJERCICIO 2: Función créditos

{-
Esta funcion toma un entero y te regresa la materia que tienen esos creditos.
-}

creditos :: Int -> [String]
creditos n = if n == 12 then ["Introduccion a las Ciencias de la Computacion", "Matematicas para las Ciencias Aplicadas I"]
             else if n == 12 then ["Estructuras Discretas", "Algebra Superior I"]  
             else if n == 10 then ["Calculo Diferencial e Integral I"]
             else if n == 4 then ["Ingles I"]
             else ["No hay materias con esos creditos"]

-- EJERCICIO 3: Función negativos

{-
Esta función cuenta los numeros negativos de una lista usando compresión de listas.
-}

negativos :: [Int] -> Int
negativos xs = length [x | x <- xs, x < 0]

-- EJERCICIO 4: Funcion primos

{-
Esta función auxiliar que verifica si un numero es primmo.
-}

esPrimo :: Int -> Bool
esPrimo n = if n <= 1 then False
            else if n == 2 then True
            else if n `mod` 2 == 0 then False
            else noDivisible n 3
{-
Esta funcion auxiliar que verifica si n NO divisores desde d sqrt (n)
-}

noDivisible :: Int -> Int -> Bool
noDivisible n d = if d * d > n then True
                  else if n `mod` d == 0 then False
                  else noDivisible n (d + 2)

{-
Esta funcion principal que devuelve todos los numeros primos de una lista.
-}

primos :: [Int] -> [Int]
primos xs = [x | x <- xs, esPrimo x]

-- EJERCICIO 5: Funcion conjuntoLista

{-
Esta funcion auxiliar que verifica si un elemento esta es una lista.
-}

contiene :: Eq a => a -> [a] -> Bool
contiene x [] = False
contiene x (y:ys) = if x == y then True
                    else contiene x ys

{-
Esta funcion auxiliar que elimina duplicados usando solo operaciones permitidas.
-}

quitarDuplicados :: Eq a => [a] -> [a]
quitarDuplicados [] = []
quitarDuplicados (x:xs) = if contiene x xs then quitarDuplicados xs
                          else x : quitarDuplicados xs

{-
Esta función principal recibe una lista y devuelve una lista sin elementos repetidos.
-}

conjuntoLista :: Eq a => [a] -> [a]
conjuntoLista xs = quitarDuplicados xs