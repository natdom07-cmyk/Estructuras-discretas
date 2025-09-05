-- 1) sayHello
-- Solo podemos regresar la cadena directamente.
sayHello :: String
sayHello = "Hello, Haskell!"

-- 2) calcularPropina
calcularPropina :: Double -> Double
calcularPropina cuenta = cuenta * 0.10

-- 3) menor de tres números (sin min ni tupla)
menor3 :: Int -> Int -> Int -> Int
menor3 x y z =
  if x <= y && x <= z then x
  else if y <= x && y <= z then y
  else z

-- 4) decide
decide :: Bool -> String -> String -> String
decide condicion a b =
  if condicion then a else b

-- 5) esDescendiente (4 enteros en orden estrictamente descendente)
esDescendiente :: Int -> Int -> Int -> Int -> Bool
esDescendiente x y z w =
  if x > y && y > z && z > w
     then True
     else False

-- 6) esDivisible usando mod
esDivisible :: Int -> Int -> String
esDivisible _ 0 = "No se puede dividir entre cero"
esDivisible x y =
  if x `mod` y == 0
     then show x ++ " es divisible por " ++ show y
     else show x ++ " no es divisible por " ++ show y

-- 7) hipotenusa usando sqrt
hipotenusa :: Double -> Double -> Double
hipotenusa b h = sqrt (b*b + h*h)

-- 8) pendiente (recibe coordenadas como 4 parámetros)
pendiente :: Double -> Double -> Double -> Double -> Double
pendiente x1 y1 x2 y2 =
  if x2 == x1
     then error "Pendiente indefinida"
     else (y2 - y1) / (x2 - x1)

-- 9) distancia entre dos puntos en R^2
distanciaPuntos :: Double -> Double -> Double -> Double -> Double
distanciaPuntos x1 y1 x2 y2 =
  sqrt ((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1))

-- 10) cuadrados de una lista (con recursión)
cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = (x*x) : cuadrados xs
