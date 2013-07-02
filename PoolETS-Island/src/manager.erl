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
-module(manager).
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

start(Pools, Profiler) ->
  spawn(manager, init, [Pools, Profiler]).

init(Pools, Profiler) ->
  Profiler ! {initEvol, now()},
  lists:foreach(fun(P) -> P ! {setPoolsManager, self()}, P ! sReps, P ! sEvals end, Pools),
  loop(Pools, false, Profiler, 0).

loop(Pools, EndEvol, Profiler, NumberOfEvals) ->

  receive

    {solutionReachedByPoolManager, _} ->
      lists:foreach(fun(P) -> P ! solutionReachedbyAny end, Pools),
      loop(Pools, EndEvol, Profiler, NumberOfEvals);

    {endEvol, T} ->
      if EndEvol -> ok;
        true ->
          Profiler ! {endEvol, T, NumberOfEvals}
      end,
      loop(Pools, true, Profiler, NumberOfEvals);

    {evalDone, _} ->
      loop(Pools, EndEvol, Profiler, NumberOfEvals + 1);

    {poolManagerEnd, Pid} ->
      NewPools = lists:delete(Pid, Pools),
      LPools = length(NewPools),
      if
        (LPools =:= 0) -> self() ! finalize;
        true -> ok
      end,
      loop(NewPools, EndEvol, Profiler, NumberOfEvals);

    finalize ->
      report ! mkExperiment,
%%       io:format("Manager ended: ~p, ", [self()]),
      ok

  end.