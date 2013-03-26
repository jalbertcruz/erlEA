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
  loop({none, none}).

loop({InicioEvolucion, FinEvolucion}) ->
  receive

    {inicioEvolucion, T} ->
      loop({T, FinEvolucion});

    {finEvolucion, T} ->
      loop({InicioEvolucion, T});

    duracionEvolucion ->
      io:format("The evolution delay: ~p seconds.~n", [getSecs(InicioEvolucion, FinEvolucion)]),
      loop({InicioEvolucion, FinEvolucion});

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
