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


start(TName, Pop, IM) ->
  spawn(poolManager, init, [TName, Pop, IM]).

init(TName, Pop, IM) ->
  ets:new(TName, [named_table, set, public]),
  ets:insert(TName, [{I, none, 1} || I <- Pop]),
  CEvals = IM#configGA.evaluatorsCount,
  CReps = IM#configGA.reproducersCount,
  Evals = [evaluator:start(TName, self()) || _ <- lists:seq(1, CEvals)],
  Reps = [reproducer:start(TName, self()) || _ <- lists:seq(1, CReps)],
  loop(TName, Evals, Reps, IM, false, [], none).

loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager) ->
  receive

    {evolveDone, Pid} ->
      if SolutionReached -> Pid ! finalize;
        true ->
%% Migration
          Dds = random:uniform(),
          if Dds > 0.5 ->
            DestIdx = random:uniform(length(MigrantsDestiny)),
            Pid ! {emigrateBest, lists:nth(DestIdx, MigrantsDestiny)};
            true -> ok
          end,

          ReproducersCapacity = IM#configGA.reproducersCapacity,
          Pid ! {evolve, ReproducersCapacity}
      end,
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);

    {evalDone, Pid} ->
      if SolutionReached -> Pid ! finalize;
        true ->
          EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
          Pid ! {eval, EvaluatorsCapacity}
      end,
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);

    sReps ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      lists:foreach(fun(E) -> E ! {evolve, ReproducersCapacity} end, Reps),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);

    sEvals ->
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      lists:foreach(fun(E) -> E ! {eval, EvaluatorsCapacity} end, Evals),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);

    solutionReached ->
      if SolutionReached -> loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);
        true ->
%%           io:format("Solution reached!: ~p~n", [yes]),
%%PROFILER:
          profiler ! {endEvol, now()},
          profiler ! evolDelay,
          PoolsManager ! solutionReached,
          loop(TName, Evals, Reps, IM, true, MigrantsDestiny, PoolsManager)
      end;

    {evalEmpthyPool, Pid} ->
%      io:format("evalEmpthyPool: ~p~n", [Pid]),
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      timer:send_after(50, Pid, {eval, EvaluatorsCapacity}),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);

    {repEmpthyPool, Pid} ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      timer:send_after(50, Pid, {evolve, ReproducersCapacity}),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);

    {migrantsDestiny, Dests} ->
      loop(TName, Evals, Reps, IM, SolutionReached, Dests, PoolsManager);

    {migration, {I, F}} ->
      ets:insert(TName, {I, F, 2}),
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PoolsManager);

    {setPoolsManager, PManager} ->
      loop(TName, Evals, Reps, IM, SolutionReached, MigrantsDestiny, PManager);

    finalize ->
      ets:delete(TName),
      ok

  end.