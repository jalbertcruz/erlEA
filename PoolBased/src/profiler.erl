%% 
%% Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
%% Copyright 2013 by José Albert Cruz Almaguer.
%% 
%% This program is licensed to you under the terms of version 3 of the
%% GNU Affero General Public License. This program is distributed WITHOUT
%% ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
%% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
%% AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
%% 

-module(profiler).
-compile(export_all).

start() ->
  Pid = spawn(profiler, init, []),
  register(profiler, Pid),
  ok.

init() ->
  loop({none, none}, []).

loop({InicioEvolucion, FinEvolucion}, Iterations) ->
  receive

    {iteration, Population, Fit} ->
      PopExtWFit = [Fit(I) || I <- Population],
      Min = lists:min(PopExtWFit),
      Max = lists:max(PopExtWFit),
      Total = lists:foldl(fun(X, Sum) -> X + Sum end, 0, PopExtWFit),
      Ave = Total / length(Population),
      loop({InicioEvolucion, FinEvolucion}, [{Min, Max, Ave} | Iterations]);

    {inicioEvolucion, T} ->
      loop({T, FinEvolucion}, Iterations);

    {finEvolucion, T} ->
      loop({InicioEvolucion, T}, Iterations);

    duracionEvolucion ->
      io:format("The evolution delay: min || max || average ~n"),
      lists:foreach(fun({Min, Max, Ave}) -> io:format("~p || ~p || ~p ~n", [Min, Max, Ave]) end, lists:reverse(Iterations)),
      io:format("The evolution delay: ~p seconds.~n", [getSecs(InicioEvolucion, FinEvolucion)]),
      loop({InicioEvolucion, FinEvolucion}, Iterations);

    terminar ->
      ok

  end.

getSecs({_, S1, MicroS1}, {_, S2, MicroS2}) ->
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
