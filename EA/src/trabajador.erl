%% 
%% Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
%% Copyright 2011 by José Albert Cruz Almaguer.
%% 
%% This program is licensed to you under the terms of version 3 of the
%% GNU Affero General Public License. This program is distributed WITHOUT
%% ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
%% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
%% AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
%% 
%% 
%% @doc Módulo que implementa el comportamiento de los trabajadores.
%% 
-module(trabajador).
-compile(export_all).

start(Fitness) ->
    Pid = spawn(trabajador, init,[Fitness]),
    Pid.

init(Fitness)->
    loop(Fitness).



loop(Fitness) ->
  receive

      %% Mensaje que le ordena a un trabajador realizar el cálculo.
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
