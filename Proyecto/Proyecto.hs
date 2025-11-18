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


{-
 - construirArbolHuffman: Construye árbol expandiendo hoja izquierda más profunda
 - Proceso: ordenar frecuencias → tomar primer elemento → expandir secuencialmente
 -}
construirArbolHuffman :: String -> ArbolHuffman
construirArbolHuffman texto = 
  case frecuencias of
    [] -> Vacio
    [(c, f)] -> AB (Hoja c f) Vacio Vacio
    (primero:resto) -> insertarSecuencial resto arbolInicial
      where
        arbolInicial = AB (Hoja (fst primero) (snd primero)) Vacio Vacio
  where
    frecuencias = calcularFrecuencias texto

{-
 - insertarSecuencial: Inserta cada carácter secuencialmente en el árbol
 - expandiendo siempre la hoja más a la izquierda y profunda
 -}
insertarSecuencial :: [(Char, Int)] -> ArbolHuffman -> ArbolHuffman
insertarSecuencial [] arbol = arbol
insertarSecuencial ((c, f):resto) arbol = 
  insertarSecuencial resto arbolExpandido
  where
    arbolExpandido = expandirHojaIzquierda arbol c f

{-
 - expandirHojaIzquierda: Encuentra y expande la hoja más a la izquierda y profunda
 - La hoja encontrada se reemplaza por un nodo con:
 -   - Izquierda: nueva hoja con el carácter a insertar
 -   - Derecha: la hoja original
 -}
expandirHojaIzquierda :: ArbolHuffman -> Char -> Int -> ArbolHuffman
expandirHojaIzquierda Vacio c f = AB (Hoja c f) Vacio Vacio
expandirHojaIzquierda (AB nodo Vacio Vacio) c f =
  let frecuenciaNueva = obtenerFrecuencia nodo + f
      nuevaHoja = Hoja c f
      nuevoNodo = Nodo frecuenciaNueva
  in AB nuevoNodo (AB nuevaHoja Vacio Vacio) (AB nodo Vacio Vacio)
expandirHojaIzquierda (AB nodo izq der) c f =
  AB nodo (expandirHojaIzquierda izq c f) der