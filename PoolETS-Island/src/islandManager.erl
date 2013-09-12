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

-module(islandManager).
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

start() ->
  spawn(islandManager, init, []).

init() ->
  loop(none).

loop(D) ->
  receive

    start ->
%%       io:format("start ~n", []),
      D#islandManager.profiler ! {initEvol, now()},
      lists:foreach(
        fun(P) ->
          P ! sReps,
          P ! sEvals
        end,
        D#islandManager.pools),

      loop(D);

    {init, Conf} ->
      loop(Conf#islandManager{endEvol = false, numberOfEvals = 0, solutions = []});

    {evalDone, Pid, N} ->
%%       io:format("evalDone: ~p ~n", [N]),
      Res = lists:member(Pid, D#islandManager.pools),
      if
        Res ->
          loop(D#islandManager{numberOfEvals = D#islandManager.numberOfEvals + N});

        true ->
          loop(D)
      end ;

    {poolManagerEnd, Pid} ->
%%       io:format("poolManagerEnd ~n", []),
      Res = lists:member(Pid, D#islandManager.pools),
      NewPools = if
        Res ->
          lists:delete(Pid, D#islandManager.pools);
        true ->
          D#islandManager.pools
      end,
      LNewPools = length(NewPools),
      if
        (LNewPools == 0) ->
          self() ! finalize;
        true ->
          ok
      end,
      loop(D#islandManager{pools = NewPools});

    deactivate ->
      lists:foreach(
        fun(P) ->
          P ! deactivate
        end,
        D#islandManager.pools
      ),
      loop(D);

    {solutionReached, Pid, {Ind, Fit}} ->
%%       io:format("solutionReached ~n", []),
      self() ! deactivate,
      EndEvol = D#islandManager.endEvol,
      if
        EndEvol -> ok;
        true ->
          D#islandManager.profiler ! {endEvol, now(), D#islandManager.numberOfEvals, Fit},
          loop(D#islandManager{endEvol = true})
      end,
      loop(D);

    {numberOfEvaluationsReached, Pid, BestSol} ->
      NewPools = lists:delete(Pid, D#islandManager.pools),
      LPools = length(NewPools),
      CurrentSolutions = [BestSol | D#islandManager.solutions],
      if

        (LPools == 0) ->
          {_, Fit} = bestSolution(CurrentSolutions),
          D#islandManager.profiler ! {endEvol, now(), D#islandManager.numberOfEvals, Fit},
          Pid ! finalizeAllWorkers,
          loop(D#islandManager{endEvol = true, pools = [], solutions = CurrentSolutions});

        true ->
          Pid ! finalizeAllWorkers,
          loop(D#islandManager{pools = NewPools, solutions = CurrentSolutions})
      end;

    finalize ->
      D#islandManager.profiler ! experimentEnd,
      ok

  end.

bestSolution([Head | Rest]) ->
  lists:foldl(
    fun({I1, F1}, {I2, F2}) ->
      if
        F1 < F2 ->
          {I2, F2};

        true ->
          {I1, F1}
      end
    end, Head, Rest
  ).
