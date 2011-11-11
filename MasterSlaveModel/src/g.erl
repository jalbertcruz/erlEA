-module(g).
-compile(export_all).

s(NTrabajadores)->
    colector:start(NTrabajadores),
	profiler:start(),
    Pid = spawn(g, init,[]),
    register(g, Pid),
    ok.

s()->
    colector:start(150),
    Pid = spawn(g, init,[]),
    register(g, Pid),
	profiler:start(),
    ok.

init()->
    loop().

iterar(Seleccion, FSeleccionPares, FCruce, FMutar)->
    Pares = FSeleccionPares(Seleccion),
    NuevosIndividuos = [FCruce(I) || I <- Pares],
    TInd = lists:append(Seleccion, NuevosIndividuos),
    PuntoDeMutacion = random:uniform(length(TInd) - 1),
    {Pre, [X|Post]} = lists:split(PuntoDeMutacion - 1, TInd),
    XMutado = FMutar(X),
    [XMutado | lists:append(Pre, Post)].

loop()->
  receive
  
      {addW, N, FF} ->
	  colector ! {addW, N, FF},
	  loop();

      {remW, N} ->
	  colector ! {remW, N},
	  loop();

      {chfitness, NF} ->
	  colector ! {chfitness, NF},
	  loop();

      {calc, Generaciones} ->
	  {A1,A2,A3} = now(),
	  random:seed(A1,A2,A3),
	  profiler ! {inicioEvolucion, now()},
	  colector ! {filtrar, g:gp(50, 8), Generaciones},
	  loop();

      {calc, Poblacion, Generaciones} ->
	  {A1,A2,A3} = now(),
	  random:seed(A1,A2,A3),
	  profiler ! {inicioEvolucion, now()},
	  colector ! {filtrar, Poblacion, Generaciones},
	  loop();

      {itera, Seleccion, N} when N == 0 ->
	  Res = iterar(Seleccion, fun g:seleccionPares/1, fun g:cruce/1,fun  g:mutar/1),
	  self() ! {calculado, Res}, % Reporte de la solución
	  loop();
		
      {itera, Seleccion, N} when N > 0 ->
	  Res = iterar(Seleccion, fun g:seleccionPares/1, fun g:cruce/1,fun  g:mutar/1),
	  colector ! {filtrar, Res, N},
	  loop();

      {calculado, Res} ->
	  profiler ! {finEvolucion, now()}, %% Fin del calculo
	  io:format("Poblacion final de ~p cromosomas:~n", [length(Res)]),
	  imprimirPoblacion(Res),
	  loop();
	  
      terminar  ->
	  colector ! terminar,
	  profiler ! terminar,
	  ok

  end.

imprimirPoblacion(P)->
    Pares = [ {trabajador:fitness(I), I} || I <- P ],
    Lista = lists:keysort(1, Pares),
    lists:foreach(
      fun({F, C}) -> 
	      io:format("Cromosoma: ~p con fitness: ~p~n", [C, F])
      end, 
      Lista).

seleccionPares(P) ->
    Pivote = length(P) div 2,
    {A, B} = lists:split(Pivote, P),
    lists:zip(A, B).

% TODO: Probar varias veces
cruce(Parent1, Parent2, Pivote1, Pivote2)->
    {Frag1, _} = lists:split(Pivote1, Parent1),
    {_, Frag3} = lists:split(Pivote2, Parent1),
    Frag2 = lists:sublist(Parent2, Pivote1, Pivote2-Pivote1),
    Frag1 ++ Frag2 ++ Frag3.

% TODO: Probar varias veces
cruce({P1, P2})->
    Pivote1 = random:uniform(length(P1) - 1),
    Pivote2 = Pivote1 + random:uniform(length(P1) - Pivote1),
    cruce(P1, P2, Pivote1, Pivote2).

mutar(S)->
    MutationPoint = random:uniform(length(S)-1),
    {Pre, [X|Post]} = lists:split(MutationPoint-1, S),
    P = probability(1),
    Ngen = if P -> if X == 1 -> 0; true -> 1 end; true -> X end,
    Pre ++ [Ngen | Post].

generarCromosoma(Tam)->
    generarCromosomaAux(Tam).

generarCromosomaAux(0) -> [];
generarCromosomaAux(N) -> 
    [random:uniform(2)-1 | generarCromosomaAux(N - 1)].

generarPoblacionAux(1, NCromosomas)-> [generarCromosoma(NCromosomas)];
generarPoblacionAux(N, NCromosomas) -> [generarCromosoma(NCromosomas) | generarPoblacionAux(N - 1, NCromosomas)].

gp(Tam, NCromosomas)->
    TReal = Tam * 3,
    generarPoblacionAux(TReal, NCromosomas).

% A simple probability function that returns true with 
% a probability of 1/2^X
probability(X)-> probability(X, X).
probability(_, 0) -> true;
probability(X, A) ->
    N1 = random:uniform(),
    N2 = random:uniform(),
    case N1 > N2 of
        true -> probability(X, A-1);
        false -> false
    end.

fitness0(L) ->
    length(lists:filter(fun(X) -> X == 0 end, L)) * 2.

fitnessL(L) ->
    fitness(0, L).

fitness(Max, [])->
    Max;
fitness(Max, [0|R]) ->
    fitness(Max, R);
fitness(Max, [1|R]) ->
   {Pre, Suf}= lists:splitwith(fun(X)-> X == 1 end, [1|R]),
    fitness(max(Max, length(Pre)), Suf).