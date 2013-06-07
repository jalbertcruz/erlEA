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
  loop(none, [], [], false, none, 0).

loop(InitEvol, Iterations, Emigrations, OneRun, Conf, NIslands) ->

  receive

    {configuration, NConf, NNIslands} ->
      %io:format("Configuration arrived: ~p~n", [NConf]),
      loop(none, [], [], false, NConf, NNIslands);

    {migration, {Ind, Fit}, T} ->
      loop(InitEvol, Iterations, [{Ind, Fit, T} | Emigrations], OneRun, Conf, NIslands);

    {iteration, Population} ->
      PopExtWFit = [evaluator:maxOnes(I) || I <- Population],
      Min = lists:min(PopExtWFit),
      Max = lists:max(PopExtWFit),
      Total = lists:foldl(fun(X, Sum) -> X + Sum end, 0, PopExtWFit),
      Ave = Total / length(Population),
      loop(InitEvol, [{Min, Max, Ave} | Iterations], Emigrations, OneRun, Conf, NIslands);

    {initEvol, T} ->
      loop(T, Iterations, Emigrations, OneRun, Conf, NIslands);

    {endEvol, T} ->
      if OneRun -> ok;
        true ->
          EvolutionDelay = getSecs(InitEvol, T),
          NEmig = length(Emigrations),
%%           io:format("The evolution delay: ~p seconds.~n", [EvolutionDelay]),
%%           io:format("Number of migrations: ~p~n", [NEmig]),
          report ! {experimentEnd, EvolutionDelay, NEmig, Conf, NIslands}

      end,
      loop(InitEvol, Iterations, Emigrations, true, Conf, NIslands);

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
