# Objetivo
El objetivo del proyecto es construir un solucionador de juegos *Nonogramas*
(ver http://www.nonograms.org/) utilizando un *SAT solver*.

El esquema del solucionador es el siguiente. Dado un nonograma, se tiene
que construir una teoría proposicional &Delta; tal que:
* el nonograma tiene solución si y sólo si &Delta; es satisfacible,
* cada la solución del nonograma esta en correspondencia con los modelos de &Delta;, y vice versa.

El solucionador consiste entonces de:
1. un programa que *codifica* un nonograma dado en una teoría proposicional &Delta;,
2. llamar a un SAT solver con entrada &Delta;,
3. si el SAT solver consigue un modelo, llama un programa que *decodifica* el modelo en una solución para el nonograma.

# Material entregado

Se entrega un directorio que contiene distintos casos de prueba.

# Actividad 1

Instalar el SAT solver llamado `minisat` y estudiar el formato de entrada y salida del mismo.

# Actividad 2

Estudiar los nonogramas y el formato .non para describirlos. Construir un codificador de nonogramas en teorías proposicionales,
y un decodificador de modelos proposicionales en soluciones al nonograma.

# Actividad 3

Construir un ejecutable que dado un archivo con un caso de prueba, llama al codificador
sobre el caso de entrada, luego llama al SAT solver, y por último decodifica la
solución calculada (si existe) en una solución al nonograma.

# Actividad 4

Construir una imagen a partir de la solución al nonograma, ya que toda
solución se corresponde con un bitmap de pixels blanco y negro.

# Entregables

Se debe entregar, en el repositorio, todo el código implementado,
los resultados experimentales, y un pequeño informe, preferiblemente
en formato .md, que describa lo que hicieron y sus conclusiones.

