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
-module(experiment).
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

init() ->
  Profiler = profiler:start(),
  report:start(Profiler),

  report ! {session,
    [
      {experiment, r1, [Profiler]}, {experiment, r2, [Profiler]}
%%       ,
%%       {experiment, r3, [Profiler]}, {experiment, r4, [Profiler]},
%%       {experiment, r5, [Profiler]}, {experiment, r6, [Profiler]}
    ]}.

init(N) ->
  Profiler = profiler:start(),
  report:start(Profiler),
  LN = lists:seq(1, N),
  Exps = [
    {experiment, r1, [Profiler]}, {experiment, r2, [Profiler]},
    {experiment, r3, [Profiler]}, {experiment, r4, [Profiler]},
    {experiment, r5, [Profiler]}, {experiment, r6, [Profiler]}
  ],

  report ! {session, lists:flatten(lists:map(fun(P) -> [P || _ <- LN] end, Exps))}.

%report ! {session, [ {experiment, r3}, {experiment, r4}, {experiment, r5}, {experiment, r6}]}.

r1(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
%io:format("Sending to profiler: ~p, the configuration: ~p~n", [Profiler, Conf]),
  Profiler ! {configuration, Conf, 2},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  {Pop1, Conf1} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P2 = poolManager:start(tb2, Pop1, Conf1, Profiler),

  P1 ! {migrantsDestination, [P2]},
  P2 ! {migrantsDestination, [P1]},

  manager:start([P1, P2], Profiler).

r2(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  Profiler ! {configuration, Conf, 2},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  {Pop1, Conf1} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P2 = poolManager:start(tb2, Pop1, Conf1, Profiler),

  P1 ! {migrantsDestination, [P2]},
  P2 ! {migrantsDestination, [P1]},

  manager:start([P1, P2], Profiler).


r3(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  Profiler ! {configuration, Conf, 4},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  {Pop1, Conf1} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P2 = poolManager:start(tb2, Pop1, Conf1, Profiler),
  {Pop2, Conf2} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P3 = poolManager:start(tb3, Pop2, Conf2, Profiler),
  {Pop3, Conf3} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P4 = poolManager:start(tb4, Pop3, Conf3, Profiler),

  P1 ! {migrantsDestination, [P2, P3]},
  P2 ! {migrantsDestination, [P3, P4]},
  P3 ! {migrantsDestination, [P4, P1]},
  P4 ! {migrantsDestination, [P1, P2]},

  manager:start([P1, P2, P3, P4], Profiler).

r4(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  Profiler ! {configuration, Conf, 4},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  {Pop1, Conf1} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P2 = poolManager:start(tb2, Pop1, Conf1, Profiler),
  {Pop2, Conf2} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P3 = poolManager:start(tb3, Pop2, Conf2, Profiler),
  {Pop3, Conf3} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P4 = poolManager:start(tb4, Pop3, Conf3, Profiler),

  P1 ! {migrantsDestination, [P2, P3]},
  P2 ! {migrantsDestination, [P3, P4]},
  P3 ! {migrantsDestination, [P4, P1]},
  P4 ! {migrantsDestination, [P1, P2]},

  manager:start([P1, P2, P3, P4], Profiler).

r5(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  Profiler ! {configuration, Conf, 8},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  {Pop1, Conf1} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P2 = poolManager:start(tb2, Pop1, Conf1, Profiler),
  {Pop2, Conf2} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P3 = poolManager:start(tb3, Pop2, Conf2, Profiler),
  {Pop3, Conf3} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P4 = poolManager:start(tb4, Pop3, Conf3, Profiler),
  {Pop4, Conf4} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P5 = poolManager:start(tb5, Pop4, Conf4, Profiler),
  {Pop5, Conf5} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P6 = poolManager:start(tb6, Pop5, Conf5, Profiler),
  {Pop6, Conf6} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P7 = poolManager:start(tb7, Pop6, Conf6, Profiler),
  {Pop7, Conf7} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  P8 = poolManager:start(tb8, Pop7, Conf7, Profiler),

  P1 ! {migrantsDestination, [P2, P3]},
  P2 ! {migrantsDestination, [P3, P4]},
  P3 ! {migrantsDestination, [P4, P5]},
  P4 ! {migrantsDestination, [P5, P6]},
  P5 ! {migrantsDestination, [P6, P7]},
  P6 ! {migrantsDestination, [P7, P8]},
  P7 ! {migrantsDestination, [P8, P1]},
  P8 ! {migrantsDestination, [P1, P2]},

  manager:start([P1, P2, P3, P4, P5, P6, P7, P8], Profiler).

r6(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  Profiler ! {configuration, Conf, 8},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  {Pop1, Conf1} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P2 = poolManager:start(tb2, Pop1, Conf1, Profiler),
  {Pop2, Conf2} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P3 = poolManager:start(tb3, Pop2, Conf2, Profiler),
  {Pop3, Conf3} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P4 = poolManager:start(tb4, Pop3, Conf3, Profiler),
  {Pop4, Conf4} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P5 = poolManager:start(tb5, Pop4, Conf4, Profiler),
  {Pop5, Conf5} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P6 = poolManager:start(tb6, Pop5, Conf5, Profiler),
  {Pop6, Conf6} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P7 = poolManager:start(tb7, Pop6, Conf6, Profiler),
  {Pop7, Conf7} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  P8 = poolManager:start(tb8, Pop7, Conf7, Profiler),

  P1 ! {migrantsDestination, [P2, P3]},
  P2 ! {migrantsDestination, [P3, P4]},
  P3 ! {migrantsDestination, [P4, P5]},
  P4 ! {migrantsDestination, [P5, P6]},
  P5 ! {migrantsDestination, [P6, P7]},
  P6 ! {migrantsDestination, [P7, P8]},
  P7 ! {migrantsDestination, [P8, P1]},
  P8 ! {migrantsDestination, [P1, P2]},

  manager:start([P1, P2, P3, P4, P5, P6, P7, P8], Profiler).