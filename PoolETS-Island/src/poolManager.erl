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

-module(poolManager).
-author("jalbertcruz@gmail.com").

-include_lib("stdlib/include/ms_transform.hrl").

-include("../include/mtypes.hrl").

-compile(export_all).


evaluationsDone(D, Self) ->
  if

    D#poolManager.active ->
      BestSolution = bestSolution(D#poolManager.tableName),
      D#poolManager.manager ! {numberOfEvaluationsReached, Self, BestSolution},
%%       D#poolManager.manager ! deactivate,
      D#poolManager{active = false};

    true ->
      D
  end.

bestSolution(TableName) ->
  Sels = ets:select(TableName,
    ets:fun2ms(fun({Ind, Fit, State}) when State == 2 -> {Ind, Fit} end)),

  LSels = length(Sels),
  if
    LSels > 0 ->
      lists:foldl(
        fun({I1, F1}, {I2, F2}) ->
          if
            F1 < F2 ->
              {I2, F2};
            true ->
              {I1, F1}
          end
        end, lists:last(Sels), Sels);
    true ->
      {null, -1}
  end.

poolInfo(Table) ->
  L = length(Table),
  if L > 0 ->
    {_, Mayor} = lists:foldl(
      fun({I1, F1}, {I2, F2}) ->
        if
          F1 < F2 ->
            {I2, F2};
          true ->
            {I1, F1}
        end
      end, lists:last(Table), Table),

    {_, Menor} = lists:foldl(
      fun({I1, F1}, {I2, F2}) ->
        if
          F1 > F2 ->
            {I2, F2};
          true ->
            {I1, F1}
        end
      end, lists:last(Table), Table),

    {Mayor, Menor};

    true ->
      {none, none}
  end.


start() ->
  spawn(poolManager, init, []).

init() ->
  loop(none).

loop(D) ->
  receive

    finalizeAllWorkers ->
      lists:foreach(
        fun(E) ->
          E ! finalize
        end, D#poolManager.evals ++ D#poolManager.reps
      ),
      loop(D);

    {initEvaluations, Cant} ->
      loop(D#poolManager{evaluations = Cant});

    {init, Conf, Population} ->

      ets:new(Conf#poolManager.tableName, [named_table, set, public]),
      ets:insert(Conf#poolManager.tableName, [{I, none, 1} || I <- Population]),
      Evals = [evaluator:start() || _ <- lists:seq(1, Conf#poolManager.evaluatorsCount)],
      Reps = [reproducer:start() || _ <- lists:seq(1, Conf#poolManager.reproducersCount)],
%%       put(poolSize, length(Population)),
      lists:foreach(
        fun(A) ->
          A ! {init, self(), Conf#poolManager.profiler}
        end, Evals ++ Reps),

      loop(Conf#poolManager{reps = Reps, evals = Evals, active = true});

    {updatePool, NewPool} -> % reemplaza una por otra (NewPool: list())
      ets:delete_all_objects(D#poolManager.tableName),
      ets:insert(D#poolManager.tableName, NewPool),
      loop(D);

    {add2Pool, NewPool} -> % Adiciona NewPool: list()
      ets:insert(D#poolManager.tableName, NewPool),
      loop(D);

    {migrantsDestination, Dests} ->
      loop(D#poolManager{migrantsDestination = Dests});

    {migration, {Ind, Fit}} ->
      ets:insert(D#poolManager.tableName, {Ind, Fit, 2}),
      loop(D);

    {evaluatorFinalized, Pid} ->
      Res = lists:member(Pid, D#poolManager.evals),
      REvals = if
                 Res ->
                   lists:delete(Pid, D#poolManager.evals);
                 true ->
                   D#poolManager.evals
               end,
      LReps = length(D#poolManager.reps),
      LEvals = length(REvals),
      if
        (LEvals == 0) and (LReps == 0) ->
          self() ! finalize;
        true -> ok
      end,
      loop(D#poolManager{evals = REvals});

    {reproducerFinalized, Pid} ->
      Res = lists:member(Pid, D#poolManager.reps),
      RReps = if
                Res ->
                  lists:delete(Pid, D#poolManager.reps);
                true ->
                  D#poolManager.reps
              end,

      LEvals = length(D#poolManager.evals),
      LReps = length(RReps),
      if
        (LEvals == 0) and (LReps == 0) ->
          self() ! finalize;
        true -> ok
      end,

      loop(D#poolManager{reps = RReps});

    {evolveDone, Pid} ->
      if D#poolManager.active ->
%% Migration
        Dds = random:uniform(),
        if Dds > 0.5 ->
          DestIdx = random:uniform(length(D#poolManager.migrantsDestination)),
          Pid ! {emigrateBest, D#poolManager.tableName, lists:nth(DestIdx, D#poolManager.migrantsDestination)};
          true -> ok
        end,

        Pid ! {evolve, D#poolManager.tableName, D#poolManager.reproducersCapacity};

        true ->
          Pid ! finalize
      end,
      loop(D);

    {evalDone, Pid, N} ->

      {DResult, CurrentEvaluations} =
        if

          D#poolManager.active ->
            D#poolManager.manager ! {evalDone, self(), N},

            {EvaluatorsCapacity, NewEvaluations} =
              case problem:terminationCondition() of
                fitnessTerminationCondition ->
                  {problem:evaluatorsCapacity(), D#poolManager.evaluations};

                _ -> %% else
                  RNewEvaluationsTemp = D#poolManager.evaluations - N,

                  RNewEvaluations = if
                                      RNewEvaluationsTemp < 0 ->
                                        0;
                                      true ->
                                        RNewEvaluationsTemp
                                    end,

                  {min(RNewEvaluations, problem:evaluatorsCapacity()), RNewEvaluations}
              end,

            RD = if
                   EvaluatorsCapacity > 0 ->
                     Pid ! {evaluate, D#poolManager.tableName, EvaluatorsCapacity},
                     D;

                   true ->
                     evaluationsDone(D, self())
                 end,

            {RD, NewEvaluations};

          true ->
            Pid ! finalize,
            {D, D#poolManager.evaluations}
        end,

      loop(DResult#poolManager{evaluations = CurrentEvaluations});

    sReps ->
      lists:foreach(fun(E) -> E ! {evolve, D#poolManager.tableName, 1} end, D#poolManager.reps),
      loop(D);

    sEvals ->
      lists:foreach(fun(E) -> E ! {evaluate, D#poolManager.tableName, 1} end, D#poolManager.evals),
      loop(D);

    deactivate ->
      loop(D#poolManager{active = false});

    {solutionReachedbyEvaluator, {Ind, Fit}, _} ->
      if
        D#poolManager.active ->

%%           io:format("solutionReachedbyEvaluator: ~p~n", [D#poolManager.tableName]),
          D#poolManager.manager ! {solutionReached, self(), {Ind, Fit}},
%%           self() ! finalize,
          loop(D#poolManager{active = false});
        true ->
          loop(D)
      end;

    evaluationsDone ->
      loop(evaluationsDone(D, self()));

    {evalEmpthyPool, Pid} ->
      if
        D#poolManager.active ->
%%           io:format("evalEmpthyPool: ~p~n", [Pid]),
          timer:send_after(100, Pid, {evaluate, D#poolManager.tableName, D#poolManager.evaluatorsCapacity});
        true ->
          Pid ! finalize
      end,
      loop(D);

    {repEmpthyPool, Pid} ->
      if
        D#poolManager.active ->
%%           io:format("repEmpthyPool: ~p~n", [Pid]),
          timer:send_after(50, Pid, {evolve, D#poolManager.tableName, D#poolManager.reproducersCapacity});
        true ->
          Pid ! finalize
      end,
      loop(D);

    finalize ->
%%       io:format("D: ~p~n", [D#poolManager.tableName]),
      ets:delete(D#poolManager.tableName),
      D#poolManager.manager ! {poolManagerEnd, self()},
      ok

  end.
