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
-module(clientETS).
-compile(export_all).

-include("../include/mtypes.hrl").


init(IM, Capacity, SolutionNotReached) ->
  loop(IM, Capacity, SolutionNotReached).

loop(IM, Capacity, SolutionNotReached) ->
  receive

    initEvolution ->
      self() ! requestWork,
      loop(IM, Capacity);

    requestWork ->
      if SolutionNotReached ->
        SelectIndividuals = IM#imodelGA.selectIndividuals,
        %Population = IM#imodelGA.population,
        {Inds2Send, NIndexes} = SelectIndividuals(Capacity),
        self() ! {evolve, Inds2Send, NIndexes};
        true -> ok
      end,
      loop(IM, SolutionNotReached, SolutionNotReached);

    {evolve, P, NIndexes} ->
      ParentsSelector = IM#imodelGA.parentsSelector,
      Evaluate = IM#imodelGA.evaluate,
      Recombination = IM#imodelGA.recombination,
      Mutation = IM#imodelGA.mutation,
      SelectNewPopulation = IM#imodelGA.selectNewPopulation,

      {Parents, IndNoSelected} = ParentsSelector(P, Evaluate),
      Population2 = [Recombination({I1, I2}) || {{I1, _}, {I2, _}} <- Parents],
      Population3 = Mutation(Population2),
      PopulationMutated = [{I, Evaluate(I)} || I <- Population3],
      NPopulationExt = SelectNewPopulation(PopulationMutated, {Parents, IndNoSelected}),
      self() ! {generationEnd, NPopulationExt, NIndexes},
      self() ! requestWork,
      loop(IM, Capacity, SolutionNotReached);


    {generationEnd, NewIndividuals, OldIndexes} ->
      if SolutionNotReached ->
        manager ! {iteration, self()},
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
% TODO: Acceder al ets, escribir
          loop(IM#imodelGA{population = NPopulation}, true);
          true ->
            profiler ! {finEvolucion, now()},
            profiler ! duracionEvolucion,
            io:format("Solution reached: ~p~n", [Solution]),
            manager ! printResults,
            % TODO: Notificarles a los otros clients que se acabó
            loop(IM, false)
        end;
        true -> ok
      end,
      loop(IM, SolutionNotReached);


    finalize ->
      ok

  end.

