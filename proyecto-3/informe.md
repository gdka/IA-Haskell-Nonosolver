Universidad Simón Bolívar <br>
Augusto Hidalgo 13-10665  
Génesis Kufatty 13-10708  
Mathías San Miguel 13-11310  

## Informe Proyecto III

### Resumen
Para este proyecto realizamos un programa en Haskell que dado un nonograma, generá la teoría preoposicional, llama  a minisat y descodifica el resultado del llamado para generar una imagen con la solución que encontró (en dado caso de que exista).

### Variables
Se utilizaron las siguientes variables para modelar la teoría proposicional &Delta;:

| Variables | Descripción |
| ------   | --------------------------------------------------------- |
| P(i,j)   | La celda de fila i de la columna j esta encendida.        |
| F(i,j,k) | El cuadro número k de la fila i comienza en la columna j. |
| C(i,j,k) | El cuadro número k de la columna j comienza en la fila i. |

### Fórmulas

Describiremos las restricciones sólo para las filas, ya que las restricciones para las columnas son equivalentes.

#### Existencia de un inicio para un bloque

Sabemos que cada bloque de una fila seleccionada debe empezar exactamente en alguna columna. Dada una fila $${i}$$ y un bloque $${k}$$ podemos chequear si el bloque $${k}$$ empieza en exactamente en una columna, con la siguiente fórmula en CNF:

$${l_({i,k}):=(F_({i,j,1}) \vee \ldots \vee F_({i,j,c})  \bigwedge_{x,y \in \{1,\ldots,c\}}_{x \neq y} \neg F_({i,j,x}) \vee \neg F_({i,j,y}}$$

Ahora verificamos que esta condición se cumpla para todos los posibles pares de la forma ($${i}$$,$${k}$$) con la siguiente fórmula en CNF:

$${\alpha:= \bigwedge l_({i,k})}$$  with: $${i \in \{1,\ldots,f\}}$$ $${k \in \{1,\ldots,n\}}$$ 

*Siendo* $${n_i}$$ *la cantidad de bloques en la fila*  $${i}$$ 

#### Inicios de bloques y celdas encendidas

Sabemos que si un bloque $${k}$$ de una fila $${i}$$ empieza en una columna $${j}$$ debe cumplir que todas las celdas desde la posicion  $${j}$$ hasta la posición $${l}$$ deben estar encendidas y en dado caso que existan las celdas exteriores al bloque, éstas deben estar apagadas. Dada una tupla ($${i}$$,$${j}$$,$${k}$$)  las siguientes fórmula en CNF resuelve este problema:

*Definimos* $${l}$$ *como la posición final de un bloque dado*

$${t_({i,j,k}):= \bigwedge_{y \in \{j,\ldots,l\}}(\sim F_({i,j,k}) 	\vee P_({i,y})) }$$ 

$${u_({i,j,k}):= \sim F_({i,j,k}) 	\vee P_({i,y-1})}$$ 

$${v_({i,j,k}):=  \sim F_({i,j,k}) 	\vee P_({i,l})}$$  


Ahora verificamos que esta condición se cumpla para todos los posibles pares de la forma ($${i}$$,$${j}$$,$${k}$$) con la siguiente fórmula en CNF:

$${\beta:= \bigwedge (t_({i,j,k}) \wedge u_({i,j,k}) \wedge v_({i,j,k}))}$$   with: $${i \in \{1,\ldots,f\}}$$ $${j \in \{1,\ldots,c\}}$$ $${k \in \{1,\ldots,n_i\}}$$ 

#### Inicios de bloques y celdas encendidas

Sabemos que si un bloque $${k}$$ de una fila $${i}$$ empieza en una columna $${j}$$ debe cumplir que todas las celdas desde la posicion  $${j}$$ hasta la posición $${l}$$ deben estar encendidas y en dado caso que existan las celdas exteriores al bloque, éstas deben estar apagadas. Dada una tupla ($${i}$$,$${j}$$,$${k}$$)  las siguientes fórmula en CNF resuelve este problema:

*Definimos* $${l}$$ *como la posición final de un bloque dado*

$${t_({i,j,k}):= \bigwedge_{y \in \{j,\ldots,l\}}(\sim F_({i,j,k}) 	\vee P_({i,y})) }$$ 

$${u_({i,j,k}):= \sim F_({i,j,k}) 	\vee P_({i,y-1})}$$ 

$${v_({i,j,k}):=  \sim F_({i,j,k}) 	\vee P_({i,l})}$$  


Ahora verificamos que esta condición se cumpla para todos los posibles pares de la forma ($${i}$$,$${j}$$,$${k}$$) con la siguiente fórmula en CNF:

$${\beta:= \bigwedge (t_({i,j,k}) \wedge u_({i,j,k}) \wedge v_({i,j,k}))}$$   with: $${i \in \{1,\ldots,f\}}$$ $${j \in \{1,\ldots,c\}}$$ $${k \in \{1,\ldots,n_i\}}$$ 


#### Inicios de bloques y celdas encendidas

Sabemos que si un bloque $${k}$$ de una fila $${i}$$ empieza en una columna $${j}$$ debe cumplir que todas las celdas desde la posicion  $${j}$$ hasta la posición $${l}$$ deben estar encendidas y en dado caso que existan las celdas exteriores al bloque, éstas deben estar apagadas. Dada una tupla ($${i}$$,$${j}$$,$${k}$$)  las siguientes fórmula en CNF resuelve este problema:

*Definimos* $${l}$$ *como la posición final de un bloque dado*

$${t_({i,j,k}):= \bigwedge_{y \in \{j,\ldots,l\}}(\sim F_({i,j,k}) 	\vee P_({i,y})) }$$ 

$${u_({i,j,k}):= \sim F_({i,j,k}) 	\vee P_({i,y-1})}$$ 

$${v_({i,j,k}):=  \sim F_({i,j,k}) 	\vee P_({i,l})}$$  


Ahora verificamos que esta condición se cumpla para todos los posibles pares de la forma ($${i}$$,$${j}$$,$${k}$$) con la siguiente fórmula en CNF:

$${\beta:= \bigwedge (t_({i,j,k}) \wedge u_({i,j,k}) \wedge v_({i,j,k}))}$$   with: $${i \in \{1,\ldots,f\}}$$ $${j \in \{1,\ldots,c\}}$$ $${k \in \{1,\ldots,n_i\}}$$ 

#### Orden de los bloques

Sabemos que si el bloque $${k}$$ de una fila $${i}$$ que empieza en una columna $${j}$$ se debe cumplir que el bloque que va después de $${k}$$ debe empezar en alguna columna válida que este a la derecha que la última columna tomada por el bloque $${k}$$. Dada una tupla ($${i}$$,$${j}$$,$${k}$$) la siguiente fórmula en CNF resuelve este problema:

$${w_({i,j,k}):= (\sim F_({i,j,k}) \vee F_({i,nl,k}),\ldots,F_({i,c,k}))}$$ 

*Definimos* $${nl}$$ *como la primera posición que no ocupa el bloque * $${k}$$

Ahora verificamos que esta condición se cumpla para todos los posibles pares de la forma ($${i}$$,$${j}$$,$${k}$$) con la siguiente fórmula en CNF:

$${\gamma:= \bigwedge (w_({i,j,k}))}$$   with: $${i \in \{1,\ldots,f\}}$$ $${j \in \{1,\ldots,c\}}$$ $${k \in \{1,\ldots,n_i\}}$$ 


#### Celdas encendidas por alguna razón

Sabemos que si una celda ($${i}$$,$${j}$$) esta encendida, debe existir algún bloque que la contenga. Dado un par ($${i}$$,$${j}$$) la siguiente fórmula en CNF resuelve este problema:

$${x_({i,j}):= (\sim P_({i,j}) \vee (\bigvee_{x,y \in \{1,\ldots,c\}} \wedge {1,\ldots,n_{i}\} \wedge {j \ge x} \wedge {j \le l}\, F({i,x,y})}$$ 

$${\delta:= \bigwedge (x_({i,j}))}$$   with: $${i \in \{1,\ldots,f\}}$$ $${j \in \{1,\ldots,c\}}$$

### Resultados y Conlusiones

Se lograron construir casi todas las imagenes (puede verse en el github, en donde estan montadas todas las imagenes generadas). Hubieron cuatro casos que nuestro programa no terminó. 

Podemos concluir que los resultados no logrados fueron por el tamaño de los casos y la gran cantidad de conflictos que se generan entre las reglas, lo que puede ocasionar mucho backtraking, lo cual retrasa la corrida significativamente.
