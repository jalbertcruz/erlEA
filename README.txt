Prerrequisitos para el uso del proyecto IslandModel

- Erlang R15B
- Ruby 1.9
- JDK 1.7
	Bibliotecas
		- gson 2.1 (http://code.google.com/p/google-gson/)
		- jinterface 1.5.5 (en Libs de la instalación de Erlang)
		- Rserve.jar, REngine.jar (del paquete Rserve de R)
		
- R 2.14.1
	- Rserve 0.6-6 (http://cran.r-project.org/web/packages/Rserve/index.html)

	
Para correrlo:

1. Descompactar packages/Manager.7z
	1.1 - En el fichero config.json especificar el IP (<IP local>) de la PC en la que se correrá el experimento
	
2. Ir al fichero ./MasterSlaveModel/src/experiment.erl y poner en la última línea el mismo IP especificado en 1.1

3. Ir a la raíz y compilar:
	compile.rb

4. cd ./MasterSlaveModel/src

4.1 Ejecutar:
	erl -name master@<IP local> -setcookie pas

5. Ejecutar las instrucciones R:
	library(Rserve)
	Rserve()
	
6. Ejecutar el programa Manager.jar
	
7. En el shell de Erlang obtenido en 4.1 ordenar:
	T = experiment:run().

8. Esperar a que el experimento acabe para que se muestren los resultados en una gráfica R que aparecerá en la interfaz de Manager.

9. Ejecuto: 
	T().
	
   Para terminar con la ejecución de los procesos.

Notas: En el directorio donde esté Manager.jar aparecerá el script R (result.r) con el que se generó el gráfico.