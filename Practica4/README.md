# Práctica 04: Números Naturales y recursión

## Objetivo
Implementar funciones recursivas sobre estructuras de datos algebraicos, específicamente para números naturales y matrioskas, aplicando los conceptos de recursión estructural, casos base y casos recursivos para construir operaciones matemáticas básicas desde cero.

## Descripción de casos base y recursivos para cada función

### 1. a_natural (Int → Natural)
**Caso base:** Cuando recibo el entero 0, retorno `Cero` directamente porque `Cero` es la representación natural del número 0 en nuestro sistema.

**Caso recursivo:** Para cualquier entero n > 0, utilizo el constructor S (sucesor) sobre el resultado de transformar (n-1). La idea es que, si quiero obtener la representación natural de n, debo considerar que n equivale a (n-1) + 1. Por lo tanto, Natural(n) se define como S(Natural(n-1)).

### 2. a_entero (Natural → Int)  
**Caso base:** `Cero` se convierte directamente al entero 0, porque `Cero` representa exactamente el número cero.

**Caso recursivo:** Cuando tengo S n, esto indica que es el sucesor de n, así que su valor entero corresponde a 1 + (valor entero de n). En otras palabras, cada aparición del constructor S equivale a “sumar uno”, por lo que el número final se obtiene acumulando esas sumas.

### 3. potenciaNat (Natural → Natural → Natural)
**Caso base:** Cualquier número elevado a la potencia `Cero` es `S Cero` (que representa 1). Esto sigue la regla matemática universal: x^0 = 1.

**Caso recursivo:** Para calcular base^(S exp), aplico la identidad base^(n+1) = base × base^n. La lógica detrás es que elevar a una potencia mayor implica multiplicar una vez más por la base. Entonces, multiplico la base por el resultado de base^exp.

### 4. facNat (Natural → Natural)
**Caso base:** El factorial de `Cero` es `S Cero` (representa 1), siguiendo la definición matemática 0! = 1.

**Caso recursivo:** Para calcular (S n)!, empleo la fórmula (n+1)! = (n+1) × n!. Esto significa que el factorial de un número consiste en multiplicar ese número por el factorial del anterior. En consecuencia, multiplico (S n) por facNat(n).

### 5. mayorIgual (Matrioska → Matrioska → Bool)
**Casos base:** 
- `Mati` es igual a `Mati` porque ambas representan la muñeca más pequeña (0 capas)
- `Mati` es menor que cualquier `Cont` porque Mati no tiene capas (0) y Cont tiene al menos una capa (≥1)
- Cualquier `Cont` es mayor que `Mati` por la razón inversa (≥1 capas > 0 capas)

**Caso recursivo:** Para comparar Cont m1 con Cont m2, realizo una comparación recursiva entre m1 y m2. La idea es que, si ambas matrioskas tienen una capa exterior, su tamaño real depende de lo que contienen dentro. Es como quitar la cubierta más externa de ambas y evaluar lo que queda.

### 6. aplana (Matrioska → [Matrioska])
**Caso base:** `Mati` se convierte en la lista `[Mati]` porque es la muñeca más pequeña y no hay nada más que "desempacar".

**Caso recursivo:** Para un Cont m, agrego (Cont m) al inicio de la lista y concateno ese valor con el resultado de aplanar m. La idea es que primero debo registrar la capa externa y luego continuar hacia adentro, mostrando capa por capa en orden desde afuera hasta la muñeca más pequeña.

## Tiempo requerido
Aproximadamente 2-3 horas para implementar todas las funciones, entender los conceptos de recursión estructural y realizar pruebas.
