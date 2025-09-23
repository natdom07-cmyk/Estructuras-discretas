
module Aux (Natural(..), sumaNat, multiplicaNat) where

data Natural = Cero | S Natural deriving (Eq, Show)

{- 
FUNCIÓN AUXILIAR: sumaNat
Propósito: Sumar dos números naturales
Retorna: Un Natural que es la suma
-}

sumaNat :: Natural -> Natural -> Natural
sumaNat Cero n = n  
-- Caso base: Si el primer número es Cero, la suma es simplemente n


sumaNat (S m) n = S (sumaNat m n)  
-- Caso recursivo: Si el primer número es S m (es decir, m+1) entonces (m+1) + n = S(m + n) 

{-
FUNCIÓN AUXILIAR: multiplicaNat  
Propósito: Multiplicar dos números naturales
Retorna: Un Natural que es el producto
-}

multiplicaNat :: Natural -> Natural -> Natural
multiplicaNat Cero _ = Cero  
-- Caso base: Si el primer número es Cero, el producto es Cero


multiplicaNat (S m) n = sumaNat n (multiplicaNat m n)  
-- Caso recursivo: Si el primer número es S m (es decir, m+1) entonces (m+1) × n = n + (m × n)
