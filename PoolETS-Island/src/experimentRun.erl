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

-module(experimentRun).
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

init() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  problem:init(),

  Profiler = profiler:start(),
  Manager = manager:start(),

  Manager ! {init, #manager{profiler = Profiler}},
  Profiler ! {init, Manager},

  Manager ! {session,
    [{experiment, r2, [Profiler, Manager]} ||
      _ <- lists:seq(1, 20)]
  }.

%% init(N) ->
%%   Profiler = profiler:start(),
%%   manager:start(Profiler),
%%   LN = lists:seq(1, N),
%%   Exps = [
%%     {experiment, r1, [Profiler]}, {experiment, r2, [Profiler]},
%%     {experiment, r3, [Profiler]}, {experiment, r4, [Profiler]},
%%     {experiment, r5, [Profiler]}, {experiment, r6, [Profiler]}
%%   ],
%%
%%   report ! {session, lists:flatten(lists:map(fun(P) -> [P || _ <- LN] end, Exps))}.
%%
%% %report ! {session, [ {experiment, r3}, {experiment, r4}, {experiment, r5}, {experiment, r6}]}.