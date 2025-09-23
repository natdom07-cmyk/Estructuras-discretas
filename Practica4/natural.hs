import Aux 

{-
FUNCIÓN 1: a_natural
Propósito: Convierte un número entero (Int) a su representación Natural
Ejemplo: a_natural 3 = S(S(S Cero))
-}

a_natural :: Int -> Natural
a_natural 0 = Cero  

a_natural n = S (a_natural (n - 1))  


{-
FUNCIÓN 2: a_entero  
Propósito: Convierte un número Natural a su representación entera (Int)
Ejemplo: a_entero (S(S(S Cero))) = 3
-}

a_entero :: Natural -> Int
a_entero Cero = 0  

a_entero (S n) = 1 + (a_entero n)  


{-
FUNCIÓN 3: potencia
Propósito: Calcula base elevado a la potencia exp (base^exp)  
Ejemplo: potencia (S(S Cero)) (S(S(S Cero))) = 2^3 = 8
-}

potencia :: Natural -> Natural -> Natural
potencia _ Cero = S Cero  

potencia base (S exp) = multiplicaNat base (potencia base exp)


{-
FUNCIÓN 4: facNat
Propósito: Calcula el factorial de un número natural (n!)
Ejemplo: facNat (S(S(S Cero))) = 3! = 6
-}

facNat :: Natural -> Natural
facNat Cero = S Cero  

facNat (S n) = multiplicaNat (S n) (facNat n)  
