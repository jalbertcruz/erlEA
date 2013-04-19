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
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

start() ->
  Pid = spawn(profiler, init, []),
  register(profiler, Pid),
  Pid.

init() ->
  loop({none, none}, []).

loop({InitEvol, EndEvol}, Iterations) ->

  receive

    {iteration, Population} ->
      PopExtWFit = [evaluator:maxOnes(I) || I <- Population],
      Min = lists:min(PopExtWFit),
      Max = lists:max(PopExtWFit),
      Total = lists:foldl(fun(X, Sum) -> X + Sum end, 0, PopExtWFit),
      Ave = Total / length(Population),
      loop({InitEvol, EndEvol}, [{Min, Max, Ave} | Iterations]);

    {initEvol, T} ->
      loop({T, EndEvol}, Iterations);

    {endEvol, T} ->
      loop({InitEvol, T}, Iterations);

    evolDelay ->
      L = length(Iterations),
      if
        L =/= 0 ->
          io:format("The evolution delay: ~nmin || max || average ~n"),
          lists:foreach(fun({Min, Max, Ave}) -> io:format(" ~p || ~p || ~p ~n", [Min, Max, Ave]) end, lists:reverse(Iterations)),
          io:format("Reproducer's iterations: ~p~n", [length(Iterations)]);
        true -> ok
      end,
      io:format("The evolution delay: ~p seconds.~n", [getSecs(InitEvol, EndEvol)]),
      io:format("Chromosome length: ~p, ", [configBuilder:chromosomeSize()]),
      io:format("number of individuals: ~p~n", [configBuilder:popSize()]),

      Conf = configBuilder:configGA(),
      io:format("Evaluators used: ~p, ", [Conf#configGA.evaluatorsCount]),
      io:format("with capacity: ~p~n", [Conf#configGA.evaluatorsCapacity]),
      io:format("Reproducers used: ~p, ", [Conf#configGA.reproducersCount]),
      io:format("with capacity: ~p~n", [Conf#configGA.reproducersCapacity]),

      loop({InitEvol, EndEvol}, Iterations);

    finalize ->
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
