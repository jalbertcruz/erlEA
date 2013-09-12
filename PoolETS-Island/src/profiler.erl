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
  spawn(profiler, init, []).

init() ->
%%   io:format("profiler started: ~p~n", [self()]),
  loop(none).

loop(D) ->

  receive

    {init, Manager} ->
      loop(#profiler{manager = Manager, emigrations = [], iterations = []});

    {configuration, NConf, NNIslands} ->
%io:format("Configuration arrived: ~p~n", [NConf]),
      loop(D#profiler{conf = NConf, nIslands = NNIslands, emigrations = [], iterations = []});

    {migration, {_, _}, T} -> % {migration, {Ind, Fit}, T} -> For future use.
      loop(D#profiler{emigrations = [T | D#profiler.emigrations]});

    {iteration, Population} ->
%%       FitnessEachInd = [problem:function/1(I) || I <- Population],
%%       Min = lists:min(FitnessEachInd),
%%       Max = lists:max(FitnessEachInd),
%%       Total = lists:foldl(fun(X, Sum) -> X + Sum end, 0, FitnessEachInd),
%%       Ave = Total / length(Population),
%%       loop(D#profiler{iterations = [{Min, Max, Ave} | D#profiler.iterations]})
      loop(D);

    {initEvol, T} ->
      loop(D#profiler{initEvol = T});

    experimentEnd ->
      EvolutionDelay = D#profiler.evolutionDelay,
      NEmig = length(D#profiler.emigrations),

      D#profiler.manager !
        {experimentEnd,
          [EvolutionDelay, NEmig, D#profiler.conf, D#profiler.nIslands, D#profiler.numberOfEvals, D#profiler.bestSolution]},

      loop(D);

    {endEvol, T, NumberOfEvals, BestSolution} ->
      EvolutionDelay = getMiliSecs(D#profiler.initEvol, T),
      loop(D#profiler{evolutionDelay = EvolutionDelay, numberOfEvals = NumberOfEvals, bestSolution = BestSolution});

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

getMiliSecs({_, S1, MicroS1}, {_, S2, MicroS2}) ->
  M = 1000000,
  N1 = S1 * M + MicroS1,
  N2 = S2 * M + MicroS2,
  (N2 - N1) div 1000.