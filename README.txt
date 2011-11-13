Prerrequisitos:

- Erlang (R14B04)
- Ruby 1.9
- JDK 1.6


0. Compilar:
	compile.rb

1. Cargar el shell de Erlang:
	cd ./MasterSlaveModel/src
	erl

2. Iniciar la aplicación:

	g:s(CantidadDeTrabajadores). %% CantidadDeTrabajadores == Número de esclavos (clientes)

3. Obtener una población aleatoria (y ponerla en la variable P):

	P = g:gp(40, 10).  %% de 120 individuos y de 10 genes cada cromosoma.

4. Mandar a correr al algoritmo en 5 generaciones:

	g ! {calc, P, 5}.

El resultado se mostrará en consola.

5. Para ver el tiempo que se tomó en trabajar:

	profiler ! duracionEvolucion. %% Inicio y fin de la corrida total.

	profiler ! duracionColectas. %% Inicio y fin de cada generación, los mensajes que informan sobre cada iteración presentan la numeración invertida (de manera que "0. 000001 segundos en la generacion 1" significa la última generación).

6. Añadir procesos clientes:

	g ! {addW, 6, fun trabajador:fitness/1}.  %% Añade 6 clientes que usaran la función trabajador:fitness/1 como función para evaluar el fitness.

7. Disminuir los procesos clientes:

	g ! {remW, 5}.  %% Elimina 5 clientes.