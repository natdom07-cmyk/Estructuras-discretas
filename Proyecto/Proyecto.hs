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

{-
 - generarTablaCodigos: Genera tabla de códigos recorriendo el árbol
 - Asigna '0' a rama izquierda, '1' a rama derecha
 -}
generarTablaCodigos :: ArbolHuffman -> [(Char, String)]
generarTablaCodigos arbol = generarCodigosAux arbol ""

{-
 - generarCodigosAux: Función auxiliar que acumula el código actual
 - mientras recorre el árbol
 -}
generarCodigosAux :: ArbolHuffman -> String -> [(Char, String)]
generarCodigosAux Vacio _ = []
generarCodigosAux (AB (Hoja c _) Vacio Vacio) codigo = [(c, codigo)]
generarCodigosAux (AB _ izq der) codigo = 
  concatenar codigosIzq codigosDer
  where
    codigosIzq = generarCodigosAux izq (concatenar codigo "0")
    codigosDer = generarCodigosAux der (concatenar codigo "1")

{-
 - obtenerCodigo: Busca el código de un carácter en la tabla
 - Devuelve el código o cadena vacía si no se encuentra
 -}
obtenerCodigo :: Char -> [(Char, String)] -> String
obtenerCodigo c tabla = 
  case buscarEnTabla c tabla of
    Just codigo -> codigo
    Nothing -> ""

{-
 - codificar: Convierte texto a binario usando codificación Huffman
 - Retorna tabla con: texto comprimido, árbol generado y tabla de códigos
 -}
codificar :: String -> (String, ArbolHuffman, [(Char, String)])
codificar texto = (textoCodificado, arbol, tabla)
  where
    arbol = construirArbolHuffman texto
    tabla = generarTablaCodigos arbol
    textoCodificado = codificarTexto texto tabla

{-
 - codificarTexto: Convierte cada carácter del texto a su código binario
 -}
codificarTexto :: String -> [(Char, String)] -> String
codificarTexto [] _ = ""
codificarTexto (c:cs) tabla = 
  concatenar (obtenerCodigo c tabla) (codificarTexto cs tabla)

{-
 - codificarSolo: Versión simplificada que solo retorna el texto codificado
 -}
codificarSolo :: String -> String
codificarSolo texto = textoCodificado
  where
    (textoCodificado, _, _) = codificar texto

{-
 - decodificar: Convierte binario a texto usando árbol de Huffman
 - Recorre árbol según bits: '0'=izquierda, '1'=derecha
 - Al llegar a hoja: guarda carácter y vuelve a raíz
 -}
decodificar :: ArbolHuffman -> String -> String
decodificar arbol bits = decodificarAux arbol arbol bits

{-
 - decodificarAux: Función auxiliar que mantiene referencia al árbol original
 - y al nodo actual mientras recorre los bits
 - 
 - Parámetros:
 - - arbolOriginal: árbol completo (para volver a la raíz)
 - - nodoActual: posición actual en el árbol
 - - bits: cadena de bits por procesar
 -}
decodificarAux :: ArbolHuffman -> ArbolHuffman -> String -> String
decodificarAux _ Vacio _ = ""
decodificarAux _ (AB (Hoja c _) Vacio Vacio) [] = [c]
decodificarAux arbolOriginal (AB (Hoja c _) Vacio Vacio) bits = 
  concatenar [c] (decodificarAux arbolOriginal arbolOriginal bits)
decodificarAux arbolOriginal (AB _ izq der) (bit:bits)
  | bit == '0' = decodificarAux arbolOriginal izq bits
  | bit == '1' = decodificarAux arbolOriginal der bits
  | otherwise  = ""
decodificarAux _ _ [] = ""

{-
 - mostrarArbol: Muestra el árbol de Huffman de forma legible
 -}
mostrarArbol :: ArbolHuffman -> String -> String
mostrarArbol Vacio indent = concatenar indent "Vacio\n"
mostrarArbol (AB (Hoja c f) Vacio Vacio) indent = 
  concatenar indent (concatenar "Hoja: '" (concatenar [c] (concatenar "' (freq: " (concatenar (show f) ")\n"))))
mostrarArbol (AB (Nodo f) izq der) indent = 
  let linea1 = concatenar indent (concatenar "Nodo (freq: " (concatenar (show f) ")\n"))
      subIzq = mostrarArbol izq (concatenar indent "  ")
      subDer = mostrarArbol der (concatenar indent "  ")
  in concatenar linea1 (concatenar subIzq subDer)

{-
 - mostrarTabla: Muestra la tabla de códigos de forma legible
 -}
mostrarTabla :: [(Char, String)] -> String
mostrarTabla [] = ""
mostrarTabla ((c, codigo):resto) = 
  concatenar "  '" (concatenar [c] (concatenar "' -> " (concatenar codigo (concatenar "\n" (mostrarTabla resto)))))

{-
 - calcularEstadisticas: Calcula bits originales y bits comprimidos
 - Retorna: (bits originales, bits comprimidos)
 -}
calcularEstadisticas :: String -> String -> (Int, Int)
calcularEstadisticas textoOriginal textoCodificado = 
  (bitsOriginales, bitsComprimidos)
  where
    bitsOriginales = longitud textoOriginal * 8
    bitsComprimidos = longitud textoCodificado

{-
 - mostrarFrecuencias: Muestra lista de frecuencias
 -}
mostrarFrecuencias :: [(Char, Int)] -> String
mostrarFrecuencias [] = ""
mostrarFrecuencias ((c, f):resto) = 
  concatenar "  '" (concatenar [c] (concatenar "' -> " (concatenar (show f) (concatenar "\n" (mostrarFrecuencias resto)))))

{-
 - ejemplo1: Ejemplo con "Viva el tocho"
 -}
ejemplo1 :: IO ()
ejemplo1 = do
  let texto = "Viva el tocho"
  putStrLn ("Ejemplo no.1: \"" ++ texto ++ "\"")
  
  putStrLn "1. Frecuencia:"
  let freqs = calcularFrecuencias texto
  putStrLn (mostrarFrecuencias freqs)
  
  putStrLn "\n2. Arbol de Huffman:"
  let arbol = construirArbolHuffman texto
  putStrLn (mostrarArbol arbol "")
  
  putStrLn "3. Tabla:"
  let tabla = generarTablaCodigos arbol
  putStrLn (mostrarTabla tabla)
  
  putStrLn "4. Codificacion:"
  let (textoCodificado, _, _) = codificar texto
  putStrLn ("  Binario: " ++ textoCodificado)
  
  let (bitsOrig, bitsComp) = calcularEstadisticas texto textoCodificado
  putStrLn ("  Bits originales : " ++ show bitsOrig)
  putStrLn ("  Bits comprimidos: " ++ show bitsComp)
  
  putStrLn "\n5. Decodificacion:"
  let textoDecodificado = decodificar arbol textoCodificado
  putStrLn ("  Texto decodificado: \"" ++ textoDecodificado ++ "\"")
  putStrLn ("  Coincide: " ++ if texto == textoDecodificado then "SI" else "NO")
  putStrLn ""

{-
 - ejemplo2: Ejemplo con otra frase
 -}
ejemplo2 :: IO ()
ejemplo2 = do
  let texto = "La gente viene y va"
  putStrLn ("Ejemplo no.1: \"" ++ texto ++ "\"")
  
  putStrLn "1. Frecuencia:"
  let freqs = calcularFrecuencias texto
  putStrLn (mostrarFrecuencias freqs)
  
  putStrLn "\n2. Arbol de Huffman:"
  let arbol = construirArbolHuffman texto
  putStrLn (mostrarArbol arbol "")
  
  putStrLn "3. Tabla:"
  let tabla = generarTablaCodigos arbol
  putStrLn (mostrarTabla tabla)
  
  putStrLn "4. Codificacion:"
  let (textoCodificado, _, _) = codificar texto
  putStrLn ("  Binario: " ++ textoCodificado)
  
  let (bitsOrig, bitsComp) = calcularEstadisticas texto textoCodificado
  putStrLn ("  Bits originales: " ++ show bitsOrig)
  putStrLn ("  Bits comprimidos: " ++ show bitsComp)
  
  putStrLn "\n5. Decodificacion:"
  let textoDecodificado = decodificar arbol textoCodificado
  putStrLn ("  Texto decodificado: \"" ++ textoDecodificado ++ "\"")
  putStrLn ("  Coincide: " ++ if texto == textoDecodificado then "SI" else "NO")
  putStrLn ""

{-
 - ejecutarEjemplos: Ejecuta todos los ejemplos
 -}
ejecutarEjemplos :: IO ()
ejecutarEjemplos = do
  ejemplo1
  ejemplo2