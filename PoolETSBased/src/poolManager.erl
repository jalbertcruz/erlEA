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
  loop(TName, Evals, Reps, IM, false).

loop(TName, Evals, Reps, IM, SolutionReached) ->
  receive
    {evolveDone, Pid} ->
      if SolutionReached -> Pid ! finalize;
        true ->
          ReproducersCapacity = IM#configGA.reproducersCapacity,
          Pid ! {evolve, ReproducersCapacity}
      end,
      loop(TName, Evals, Reps, IM, SolutionReached);

    {evalDone, Pid} ->
      if SolutionReached -> Pid ! finalize;
        true ->
          EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
          Pid ! {eval, EvaluatorsCapacity}
      end,
      loop(TName, Evals, Reps, IM, SolutionReached);

    sReps ->
      profiler ! {initEvol, now()},
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      lists:foreach(fun(E) -> E ! {evolve, ReproducersCapacity} end, Reps),
      loop(TName, Evals, Reps, IM, SolutionReached);

    sEvals ->
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      lists:foreach(fun(E) -> E ! {eval, EvaluatorsCapacity} end, Evals),
      loop(TName, Evals, Reps, IM, SolutionReached);

%%     {sendBest, Pid}->
%%         Pid ! {emigrateBest, poolBest(TName)},
%%       loop(TName, Evals, Reps, IM);

    solutionReached ->
      if SolutionReached -> loop(TName, Evals, Reps, IM, SolutionReached);
        true ->
%%           io:format("Solution reached!: ~p~n", [yes]),
%%PROFILER:
          profiler ! {endEvol, now()},
          profiler ! evolDelay,
          loop(TName, Evals, Reps, IM, true)
      end;

    {evalEmpthyPool, Pid} ->
%      io:format("evalEmpthyPool: ~p~n", [Pid]),
      EvaluatorsCapacity = IM#configGA.evaluatorsCapacity,
      timer:send_after(50, Pid, {eval, EvaluatorsCapacity}),
      loop(TName, Evals, Reps, IM, SolutionReached);

    {repEmpthyPool, Pid} ->
      ReproducersCapacity = IM#configGA.reproducersCapacity,
      timer:send_after(50, Pid, {evolve, ReproducersCapacity}),
      loop(TName, Evals, Reps, IM, SolutionReached);

    finalize ->
      ets:delete(TName),
      ok

  end.

%% poolBest(TName)->
%%   %TODO