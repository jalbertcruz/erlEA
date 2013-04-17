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
-module(poolETS).

-include("../include/mtypes.hrl").

-compile(export_all).


init(IM, SolutionNotReached) ->
  loop(IM, SolutionNotReached).

loop(IM, SolutionNotReached) ->
  receive

    {configPool, NIM} ->
      loop(NIM, true);

    {requestWork, Pid, Capacity} ->
      if SolutionNotReached ->
        SelectIndividuals = IM#imodelGA.selectIndividuals,
        Population = IM#imodelGA.population,
        {Inds2Send, NIndexes} = SelectIndividuals(Population, Capacity),
        Pid ! {evolve, Inds2Send, NIndexes};
        true -> ok
      end,
      loop(IM, SolutionNotReached);

    {resetPopulation, NPopulation} ->
      loop(IM#imodelGA{population = NPopulation}, SolutionNotReached);

    {generationEnd, NewIndividuals, OldIndexes, Pid} ->
      if SolutionNotReached ->
        manager ! {iteration, Pid},
        TerminationCondition = IM#imodelGA.terminationCondition,
        {NTerminateValue, Solution} = TerminationCondition(NewIndividuals),
        TerminateValue = not NTerminateValue,
        if TerminateValue ->
          ReplaceIndividuals = IM#imodelGA.replaceIndividuals,
%%Population = IM#imodelGA.population,
          CleanedPop = [Ind || {Ind, _} <- NewIndividuals],
          NPopulation = ReplaceIndividuals(IM, CleanedPop, OldIndexes),
%%PROFILER:
%profiler ! {iteration, NPopulation, FitFunction},

          loop(IM#imodelGA{population = NPopulation}, true);
          true ->
            profiler ! {finEvolucion, now()},
            profiler ! duracionEvolucion,
            io:format("Solution reached: ~p~n", [Solution]),
            manager ! printResults,
            loop(IM, false)
        end;
        true -> ok
      end,
      loop(IM, SolutionNotReached);

    finalize ->
      ok
  end.