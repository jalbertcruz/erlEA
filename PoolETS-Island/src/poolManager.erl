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

-include("../include/mtypes.hrl").

-compile(export_all).


start(TName, Pop, IM, Profiler) ->
  spawn(poolManager, init, [TName, Pop, IM, Profiler]).

init(TName, Pop, IM, Profiler) ->
  ets:new(TName, [named_table, set, public]),
  ets:insert(TName, [{I, none, 1} || I <- Pop]),
  CEvals = IM#configGA.evaluatorsCount,
  CReps = IM#configGA.reproducersCount,
  Evals = [evaluator:start(TName, self()) || _ <- lists:seq(1, CEvals)],
  Reps = [reproducer:start(TName, self(), Profiler) || _ <- lists:seq(1, CReps)],
  loop(TName, Evals, Reps, IM, false, [], none, Profiler).

loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler) ->
  receive

    {reproducerFinalized, Pid} ->
      NewReps = lists:delete(Pid, Reps),
      NEvals = length(Evals),
      NReps = length(NewReps),
      if
        (NEvals =:= 0) and (NReps =:= 0) -> self() ! finalize;
        true -> ok
      end,
      loop(TName, Evals, NewReps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    {evaluatorFinalized, Pid} ->
      NewEvals = lists:delete(Pid, Evals),
      NEvals = length(NewEvals),
      NReps = length(Reps),
      if
        (NEvals =:= 0) and (NReps =:= 0) -> self() ! finalize;
        true -> ok
      end,
      loop(TName, NewEvals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    {evolveDone, Pid} ->
      if SolutionReached ->
        Pid ! finalize ;
        true ->
%% Migration
          Dds = random:uniform(),
          if Dds > 0.5 ->
            DestIdx = random:uniform(length(MigrantsDestination)),
            Pid ! {emigrateBest, lists:nth(DestIdx, MigrantsDestination)};
            true -> ok
          end,

          ReproducersCapacity = IM#configGA.reproducersCapacity,
          Pid ! {evolve, ReproducersCapacity}
      end,
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    {evalDone, Pid} ->
      if SolutionReached -> Pid ! finalize;
        true ->
          EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
          PoolsManager ! {evalDone, self()},
          Pid ! {eval, EvaluatorsCapacity}
      end,
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    sReps ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      lists:foreach(fun(E) -> E ! {evolve, ReproducersCapacity} end, Reps),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    sEvals ->
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      lists:foreach(fun(E) -> E ! {eval, EvaluatorsCapacity} end, Evals),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    solutionReachedbyAny ->
      lists:foreach(fun(E) -> E ! finalize end, lists:append(Evals, Reps)),
      loop(TName, Evals, Reps, IM, true, MigrantsDestination, PoolsManager, Profiler);

    {solutionReachedbyEvaluator, _} ->
%%       io:format("SolutionReached value: ~p, in ~p~n", [SolutionReached, TName]),
      if SolutionReached -> loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);
        true ->
%%            io:format("Solution reached in!: ~p~n", [TName]),
%%PROFILER:
          PoolsManager ! {endEvol, now()},
%%           profiler ! evolDelay,
          PoolsManager ! {solutionReachedByPoolManager, self()},
          loop(TName, Evals, Reps, IM, true, MigrantsDestination, PoolsManager, Profiler)
      end;

    {evalEmpthyPool, Pid} ->
%      io:format("evalEmpthyPool: ~p~n", [Pid]),
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      timer:send_after(50, Pid, {eval, EvaluatorsCapacity}),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    {repEmpthyPool, Pid} ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      timer:send_after(50, Pid, {evolve, ReproducersCapacity}),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    {migrantsDestination, Dests} ->
      loop(TName, Evals, Reps, IM, SolutionReached, Dests, PoolsManager, Profiler);

    {migration, {I, F}} ->
      ets:insert(TName, {I, F, 2}),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PoolsManager, Profiler);

    {setPoolsManager, PManager} ->
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestination, PManager, Profiler);

    finalize ->
      ets:delete(TName),
      PoolsManager ! {poolManagerEnd, self()},
%      io:format("Table deleted: ~p~n", [TName]),
      ok

  end.