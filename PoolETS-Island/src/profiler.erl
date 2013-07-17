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
%register(profiler, Pid),
  Pid.

init() ->
  io:format("profiler started: ~p~n", [self()]),
  loop(none, [], [], none, 0).

loop(InitEvol, Iterations, Emigrations, Conf, NIslands) ->

  receive

    {configuration, NConf, NNIslands} ->
%io:format("Configuration arrived: ~p~n", [NConf]),
      loop(none, [], [], NConf, NNIslands);

    {migration, {_, _}, T} -> % {migration, {Ind, Fit}, T} -> For future use.
      loop(InitEvol, Iterations, [T | Emigrations], Conf, NIslands);

    {iteration, Population} ->
      FitnessEachInd = [evaluator:maxOnes(I) || I <- Population],
      Min = lists:min(FitnessEachInd),
      Max = lists:max(FitnessEachInd),
      Total = lists:foldl(fun(X, Sum) -> X + Sum end, 0, FitnessEachInd),
      Ave = Total / length(Population),
      loop(InitEvol, [{Min, Max, Ave} | Iterations], Emigrations, Conf, NIslands);

    {initEvol, T} ->
      loop(T, Iterations, Emigrations, Conf, NIslands);

    {endEvol, T, NumberOfEvals} ->
      EvolutionDelay = getSecs(InitEvol, T),
      NEmig = length(Emigrations),
%%           io:format("The evolution delay: ~p seconds.~n", [EvolutionDelay]),
%%           io:format("Number of migrations: ~p~n", [NEmig]),
      report ! {experimentEnd, EvolutionDelay, NEmig, Conf, NIslands, NumberOfEvals},
      loop(InitEvol, Iterations, Emigrations, Conf, NIslands);

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
  ResStr = integer_to_list(PE) ++ "." ++ FCad ++ PRL,
  list_to_float(ResStr).
