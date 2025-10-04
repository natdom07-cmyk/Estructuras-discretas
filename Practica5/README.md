# Practica 5: Listas por Recursion
---

## Objetivo

El objetivo de esta practica es implementar funciones recursivas para la manipulacion de listas en Haskell, aplicando los conceptos fundamentales de recursion vistos en clase.
---

## Tiempo Requerido

El tiempo estimado para completar esta practica es de **3 a 4 horas**, considerando:

---

## Actividades Realizadas

### Funciones Implementadas

#### 1. hollerBack
Convierte una cadena de texto a mayusculas utilizando recursion.

**Implementacion:** Recorre cada caracter de la cadena y aplica `toUpper` de la libreria `Data.Char`.

---

#### 2. palindromo
Verifica si una lista es palindromo, es decir, si se lee igual de izquierda a derecha que de derecha a izquierda.

**Implementacion:** Compara la lista original con su version invertida usando la funcion auxiliar `reversa`.

---

#### 3. replica
Genera una lista que contiene n repeticiones de un elemento dado.

**Implementacion:** Recursion simple que va decrementando el contador hasta llegar a 0.

---

#### 4. recuperaElemento
Obtiene el elemento de una lista en el indice especificado.

**Implementacion:** Recorre la lista recursivamente hasta alcanzar el indice deseado. Maneja errores para indices invalidos.

---

#### 5. rota
Rota una lista n posiciones. Si n es positivo rota a la derecha, si es negativo rota a la izquierda.

**Implementacion:** Usa funciones auxiliares para mover elementos al principio o al final, repitiendo la operacion n veces.

---

#### 6. extranio
Implementa el algoritmo de Collatz .

**Algoritmo:**
- Si n es par: dividir entre 2
- Si n es impar: multiplicar por 3 y sumar 1
- Repetir hasta llegar a 1

**Implementacion:** Recursion con guardas para verificar si el numero es par o impar, usando funciones auxiliares para las operaciones aritmeticas.

---

## Comentarios Adicionales

### Aprendizajes

- La recursion es una herramienta poderosa para trabajar con listas
- Cualquier operacion aritmetica puede construirse a partir de operaciones mas simples
- Los patrones y guardas hacen el codigo mas legible
