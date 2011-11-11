-module(colector).
-compile(export_all).

start(NTrabajadores)->
    Trabajadores = crearTrabajadores(NTrabajadores, fun trabajador:fitness/1),
    Pid = spawn(colector, init,[Trabajadores]),
    register(colector, Pid),
    ok.

init(Trabajadores) ->
  loop(Trabajadores, [], 0, [], 0).

loop(Trabajadores, ResultadosActuales, NIndividuosPoblacion, IndividuosFaltantes, Generacion) ->

    receive
	
    {chfitness, NF} ->
	    lists:foreach(fun(T) -> T ! {change, NF} end, Trabajadores),
	    loop(Trabajadores, ResultadosActuales, NIndividuosPoblacion, IndividuosFaltantes, Generacion);

    {addW, N, FF} ->
	  NuevosTrabajadores = crearTrabajadores(N, FF),
	  loop(lists:append(NuevosTrabajadores, Trabajadores), ResultadosActuales, NIndividuosPoblacion, IndividuosFaltantes, Generacion);
	
      {remW, N} ->
		  {RemTrabajadores, NuevosTrabajadores} = lists:split(N, Trabajadores),
		  apagarTrabajadores(RemTrabajadores),
		  loop(NuevosTrabajadores, ResultadosActuales, NIndividuosPoblacion, IndividuosFaltantes, Generacion);
	
	{filtrar, Poblacion, Generaciones} ->
	    profiler ! {inicioGeneracion, now()},
	    NP = length(Poblacion), NTrabajadores = length(Trabajadores),
	    DistribuidorInicial = fun(P_i, T_i)-> 
				  P1 = lists:zip(P_i, T_i),
				  lists:foreach(
				    fun(TPar) ->
 					    {Individuo, Trabajador} = TPar, 
					    Trabajador ! {calcula, Individuo}
				    end, P1)
                          end,
	    if 
		NP == NTrabajadores -> 
		    DistribuidorInicial(Poblacion, Trabajadores),
		    loop(Trabajadores, [], NP, [], Generaciones);
		NP < NTrabajadores -> % Si tengo menos cromosomas que trabajadores
		    {STrabajadores, _} = lists:split(NP, Trabajadores),
		    DistribuidorInicial(Poblacion, STrabajadores),
		    loop(Trabajadores, [], NP, [], Generaciones);
		true -> % Si tengo más cromosomas que trabajadores
		    {PrimerosIndividuos, RestoIndividuos} = lists:split(NTrabajadores, Poblacion),
		    DistribuidorInicial(PrimerosIndividuos, Trabajadores),
		    loop(Trabajadores, [], NP, RestoIndividuos, Generaciones)
	    end;


	{calculado, Cromosoma, Fitness, Pid} ->
	    Cant = length(ResultadosActuales),
	    if 
		Cant == NIndividuosPoblacion - 1 -> % Llegar el ultimo!
		    Pares = [{Fitness, Cromosoma} | ResultadosActuales],
		    lists:keysort(1, Pares),
		    P = [ P1 || {_, P1} <- Pares ],
		    NRes = length(P) div 3,
		    {_, Res} = lists:split(NRes, P), % Elimino el primer tercio
		    g ! {itera, Res, Generacion - 1},
		    profiler ! {finGeneracion, now(), Generacion}, %% Fin del calculo de los trabajadores
		    loop(Trabajadores, [], 0, [], 0);

		true -> % Cuando todavia faltan trabajadores por responder
		    NIndf = length(IndividuosFaltantes),
		    if % Si tengo cromosomas esperando trabajadores...
			NIndf > 0 ->
			    [IndividuoFaltante | ColaIndividuosFaltantes] = IndividuosFaltantes,
			    Pid ! {calcula, IndividuoFaltante},
			    loop(Trabajadores, [{Fitness, Cromosoma} | ResultadosActuales], NIndividuosPoblacion , ColaIndividuosFaltantes, Generacion);

			true -> % Sino hay cromosomas pendientes a analizar
			    loop(Trabajadores, [{Fitness, Cromosoma} | ResultadosActuales], NIndividuosPoblacion, [], Generacion)
		    end
	    end;

      terminar ->
           apagarTrabajadores(Trabajadores),
	   ok
		
    end.

crearTrabajadores(0, _) -> [];
crearTrabajadores(N, F) -> [ trabajador:start(F) | crearTrabajadores(N - 1, F) ].

apagarTrabajadores(T)->
    lists:foreach( fun(Trab)-> 
			   Trab ! parar 
		    end, 
		   T).
	