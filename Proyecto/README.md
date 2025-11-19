# Proyecto: Compresión y Descompresión con Árboles de Huffman

## Objetivo
Implementar un sistema capaz de:
- Analizar un texto de entrada.
- Calcular las frecuencias de sus caracteres.
- Construir un árbol tipo Huffman 
- Generar la tabla de códigos asociados.
- Codificar el texto a una representación binaria.
- Decodificar ese binario para reconstruir el texto original.

Este proyecto no implementa Huffman estándar, sino **la variante estructural solicitada**, lo cual produce un árbol distinto y un comportamiento característico que se documenta en esta entrega.

---

## Funcionamiento General

### 1. **Cálculo de Frecuencias**
A partir del texto, se cuentan las repeticiones de cada carácter.  
Los caracteres se ordenan alfabéticamente para establecer el orden de inserción.

### 2. **Construcción del Árbol Secuencial**
El árbol se construye siguiendo **la regla especial pedida en clase**:
- Se toma la primera frecuencia como raíz.
- Cada nueva frecuencia se inserta **expandiendo la hoja más izquierda y profunda**.
- La nueva hoja se coloca a la izquierda, y la hoja expandida pasa a la derecha.

Este método genera un árbol desbalanceado pero totalmente reproducible.

### 3. **Generación de la Tabla de Códigos**
Se recorre el árbol asignando:
- `0` para moverse a la izquierda  
- `1` para moverse a la derecha  

Cada hoja recibe un código binario único según su posición en el árbol.

### 4. **Codificación**
El texto original se transforma a binario sustituyendo cada carácter por su código.

### 5. **Decodificación**
Se recorre el árbol según los bits:
- `0` → izquierdo  
- `1` → derecho  
- Al llegar a una hoja se recupera el carácter y se reinicia desde la raíz.


### 6. **Estadísticas**
Se muestra:
- Total de bits originales (8 bits por carácter)
- Total de bits comprimidos
- Comparación entre ambos

---

## Ejecución

El proyecto contiene dos ejemplos principales:

```haskell
ejemplo1
ejemplo2

---

## Tiempo Requerido

Tarde aproximadamente 2 dia en hacerlo.
