-module(trabajador).
-compile(export_all).

start(Fitness) ->
    Pid = spawn(trabajador, init,[Fitness]),
    Pid.

init(Fitness)->
    loop(Fitness).



loop(Fitness) ->
  receive

      {calcula, Cromosoma} ->
	  colector ! {calculado, Cromosoma, Fitness(Cromosoma), self()},
          loop(Fitness);

      {change, F} ->
	  loop(F);

      parar ->
	  ok

  end.

fitness(L) ->
    length(lists:filter(fun(X) -> X == 1 end, L)).