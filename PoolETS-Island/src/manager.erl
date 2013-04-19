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

start(Pools) ->
  spawn(manager, init, [Pools]).

init(Pools) ->
  profiler ! {initEvol, now()},
  lists:foreach(fun(P)-> P ! {setPoolsManager, self()}, P ! sReps, P ! sEvals end, Pools),
  loop(Pools).

loop(Pools) ->

  receive

    solutionReached ->
      lists:foreach(fun(P)-> P ! solutionReached end, Pools),
      loop(Pools);

    finalize ->
      ok

  end.