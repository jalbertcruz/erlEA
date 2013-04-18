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
  loop(TName, Evals, Reps, IM).

loop(TName, Evals, Reps, IM) ->
  receive
    {evolveDone, Pid} ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      Pid ! {evolve, ReproducersCapacity},
      loop(TName, Evals, Reps, IM);

    {evalDone, Pid} ->
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      Pid ! {eval, EvaluatorsCapacity},
      loop(TName, Evals, Reps, IM);

    sReps ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      lists:foreach(fun(E) -> E ! {evolve, ReproducersCapacity} end, Reps),
      loop(TName, Evals, Reps, IM);

    sEvals ->
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      lists:foreach(fun(E) -> E ! {eval, EvaluatorsCapacity} end, Evals),

      loop(TName, Evals, Reps, IM);

%%     {sendBest, Pid}->
%%         Pid ! {emigrateBest, poolBest(TName)},
%%       loop(TName, Evals, Reps, IM);

    solutionReached ->
      io:format("solutionReached!!!: ~p~n", [yes]),
      self() ! finalize,
      loop(TName, Evals, Reps, IM);

    {evalEmpthyPool, Pid} ->
%      io:format("evalEmpthyPool: ~p~n", [Pid]),
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      timer:send_after(1000, Pid, {eval, EvaluatorsCapacity}),
      loop(TName, Evals, Reps, IM);

    {repEmpthyPool, Pid} ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      timer:send_after(1000, Pid, {evolve, ReproducersCapacity}),
      loop(TName, Evals, Reps, IM);

    finalize ->
      lists:foreach(fun(E) -> E ! finalize end, Evals),
      lists:foreach(fun(E) -> E ! finalize end, Reps),
      ets:delete(TName),
      ok

  end.

%% poolBest(TName)->
%%   %TODO