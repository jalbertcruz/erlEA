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
-module(profiler).
-compile(export_all).

start()->
    Pid = spawn(profiler, init,[]),
    register(profiler, Pid),
    ok.

init() ->
    loop({0, 0}, {0, 0}, []).

loop({InicioEvolucion, FinEvolucion}, {IGeneracion, FGeneracion}, LGeneraciones) ->
    receive
	
	{inicioEvolucion, T} ->
	    loop({T, FinEvolucion}, {IGeneracion, FGeneracion}, LGeneraciones);

	{finEvolucion, T} ->
	    loop({InicioEvolucion, T}, {IGeneracion, FGeneracion}, LGeneraciones);

	{inicioGeneracion, T} -> 
	    loop({InicioEvolucion, FinEvolucion}, {T, FGeneracion}, LGeneraciones);

	{finGeneracion, T, NG} -> 
	    loop({InicioEvolucion, FinEvolucion}, {0, 0}, [{IGeneracion, T, NG} | LGeneraciones]);

	duracionEvolucion  ->
	    io:format("La evolucion tomo: ~p segundos.~n", [getSecs(InicioEvolucion, FinEvolucion)]),
	    loop({InicioEvolucion, FinEvolucion}, {IGeneracion, FGeneracion}, LGeneraciones);

	duracionColectas ->
	    io:format("Las generaciones demoraron los sigientes lapsos de tiempo:~n"),
	    lists:foreach( fun(TG) -> 
				   {Ti, Tf, G}  = TG,
				   io:format("~p segundos en la generacion ~p~n", [getSecs(Ti, Tf), G])			   
			   end, LGeneraciones),
	    loop({InicioEvolucion, FinEvolucion}, {IGeneracion, FGeneracion}, []);

	terminar  -> 
	    ok

    end.

getSecs({_, S1, MicroS1}, {_, S2, MicroS2})	->
    M = 1000000,
    N1 = S1 * M + MicroS1,
    N2 = S2 * M + MicroS2,
    R = N2 - N1,
    PE = R div M,
    PR = R rem M,
    PRL = integer_to_list(PR),
    L1 = length(PRL),
    FCad = if L1 == 6 -> ""; true -> string:substr("000000", L1 + 1) end,
    integer_to_list(PE) ++ "." ++ FCad ++ PRL.