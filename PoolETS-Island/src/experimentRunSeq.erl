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

-module(experimentRunSeq).
-author("jalbertcruz@gmail.com").

-include_lib("stdlib/include/ms_transform.hrl").
-include("../include/mtypes.hrl").

-compile(export_all).

run() ->
  GAConfig = configuration:gaConfig(),
  NRes = [testsRunSeqEA() || _ <- lists:seq(1, GAConfig#gAConfig.repetitions)],
  {ok, IODevice} = file:open(GAConfig#gAConfig.seqOutputFilename, [write]),
  file:write(IODevice, "EvolutionDelay,BestSol\n"),
  lists:foreach(
    fun({EvolutionDelay, BestSol}) ->
      io:format(IODevice, "~p,~p ~n", [EvolutionDelay, BestSol])
    end,
    NRes
  ),
  file:close(IODevice),

  io:format("Ends!~n", []),
  halt().

testsRunSeqEA() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  problem:init(),

  io:format("Doing experiment (time -> ~p)~n", [now()]),
  InitEvol = now(),
  Res = runSeqEA(problem:genInitPop()),
  EndEvol = now(),
  problem:finalize(),
  {profiler:getMiliSecs(InitEvol, EndEvol), Res}.

runSeqEA(Population) ->

  ets:new(seq, [named_table, set, public]),
  ets:insert(seq, [{evaluations, problem:evaluations()}, {solutionFound, false}, {poolName, pool}]),

  ets:new(tb, [named_table, set, public]),
  ets:insert(tb, [{I, none, 1} || I <- Population]),
  Res = seqEALoop(0),

  ets:delete(seq),
  ets:delete(tb),
  Res.

seqEALoop(EvalDone) ->

  {EvaluatorsCapacity, NewEvaluations} = case problem:terminationCondition() of
    fitnessTerminationCondition ->
      {problem:evaluatorsCapacity(), ets:lookup_element(seq, evaluations, 2)};

    _ ->
      RNewEvaluationsTemp = ets:lookup_element(seq, evaluations, 2) - EvalDone,
      RNewEvaluations = if
        RNewEvaluationsTemp < 0 ->
          0;
        true ->
          RNewEvaluationsTemp
      end,

      {min(RNewEvaluations, problem:evaluatorsCapacity()), RNewEvaluations}
  end,

  ets:insert(seq, {evaluations, NewEvaluations}),

  NewEvalDone = if % Se realizan las evaluaciones.
    EvaluatorsCapacity > 0 ->
      Res = ets:select(tb,
        ets:fun2ms(fun({Ind, _, State}) when State == 1 -> Ind end),
        EvaluatorsCapacity),
      case Res of
        {Sels, _} ->
          {R, {Founded, NSels}} = evaluator:evaluate(Sels),
          if
            Founded ->
%%               [{Ind, Fit} | _] = NSels,
              ets:insert(seq, {solutionFound, true}),
              ok;
            true ->
              ok
          end,
          if
            R ->
              PNSels = [{Ind, Fit, 2} || {Ind, Fit} <- NSels],
              ets:insert(tb, PNSels),
              length(NSels);

            true ->
              0
          end;

        '$end_of_table' ->
          0
      end;

    true ->
      0
  end,
% Se realiza la reproduccion
  Sels2 = ets:select(tb,
    ets:fun2ms(fun({Ind, F, State}) when State == 2 -> {Ind, F} end)
  ),
  Subpop = reproducer:extractSubpopulation(Sels2, problem:reproducersCapacity()),

  {Res2, ResultData} = reproducer:evolve(
    Subpop,
    problem:reproducersCapacity() div 2,
    fun() -> ok end % DoWhenLittle
  ),
  if
    Res2 ->
      {NoParents, NInds, BestParents} = ResultData,

      EtsAll = dict:from_list(ets:select(tb,
        ets:fun2ms(fun({X1, Y1, Z1}) -> {X1, {Y1, Z1}} end))),

      ets:delete_all_objects(tb),
      ets:insert(tb,
        reproducer:mergeFunction(
          EtsAll, Subpop, NoParents,
          NInds, BestParents, problem:popSize())
      );

    true ->
      ok
  end,

  End = terminationCondition(),
  if
    End ->
      {_, Fit} = poolManager:bestSolution(tb),
      Fit;
    true ->
      seqEALoop(NewEvalDone)
  end.

terminationCondition() ->
  TerminationCondition = problem:terminationCondition(),
  if
    TerminationCondition == fitnessTerminationCondition ->
      ets:lookup_element(seq, solutionFound, 2);

    true ->
      ets:lookup_element(seq, evaluations, 2) == 0
  end.
