
-- Definición del tipo Matrioska
-- Representa muñecas rusas: una muñeca puede estar vacía (Mati) o contener otra muñeca (Cont)
data Matrioska = Mati | Cont Matrioska deriving (Eq, Show)

{-
FUNCIÓN 5: mayorIgual
Propósito: Compara el tamaño de dos matrioskas  
Una matrioska es "mayor" si tiene más matriokas guardadas
Retorna True si la primera es mayor o igual que la segunda
-}

mayorIgual :: Matrioska -> Matrioska -> Bool

mayorIgual Mati Mati = True  

mayorIgual Mati (Cont _) = False  

mayorIgual (Cont _) Mati = True

mayorIgual (Cont m1) (Cont m2) = mayorIgual m1 m2  


{-
FUNCIÓN 6: aplana  
Propósito: Convierte una matrioska guardada en una lista plana
La lista va de la capa más externa a la más interna
-}

aplana :: Matrioska -> [Matrioska]

aplana Mati = [Mati]  

aplana (Cont m) = (Cont m) : aplana m  
