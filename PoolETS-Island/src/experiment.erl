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
  report:start(),
  report ! {session,
    [ {experiment, r1, [Profiler]}, {experiment, r2, [Profiler]},
      {experiment, r3, [Profiler]}, {experiment, r4, [Profiler]},
      {experiment, r5, [Profiler]}, {experiment, r6, [Profiler]}
    ]}
.
%report ! {session, [ {experiment, r3}, {experiment, r4}, {experiment, r5}, {experiment, r6}]}.

r1(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  %io:format("Sending to profiler: ~p, the configuration: ~p~n", [Profiler, Conf]),
  Profiler ! {configuration, Conf, 2},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  P2 = poolManager:start(tb2, Pop, Conf, Profiler),

  P1 ! {migrantsDestiny, [P2]},
  P2 ! {migrantsDestiny, [P1]},

  manager:start([P1, P2], Profiler).

r2(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  Profiler ! {configuration, Conf, 2},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  P2 = poolManager:start(tb2, Pop, Conf, Profiler),

  P1 ! {migrantsDestiny, [P2]},
  P2 ! {migrantsDestiny, [P1]},

  manager:start([P1, P2], Profiler).


r3(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  Profiler ! {configuration, Conf, 4},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  P2 = poolManager:start(tb2, Pop, Conf, Profiler),
  P3 = poolManager:start(tb3, Pop, Conf, Profiler),
  P4 = poolManager:start(tb4, Pop, Conf, Profiler),

  P1 ! {migrantsDestiny, [P2, P3]},
  P2 ! {migrantsDestiny, [P3, P4]},
  P3 ! {migrantsDestiny, [P4, P1]},
  P4 ! {migrantsDestiny, [P1, P2]},

  manager:start([P1, P2, P3, P4], Profiler).

r4(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  Profiler ! {configuration, Conf, 4},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  P2 = poolManager:start(tb2, Pop, Conf, Profiler),
  P3 = poolManager:start(tb3, Pop, Conf, Profiler),
  P4 = poolManager:start(tb4, Pop, Conf, Profiler),

  P1 ! {migrantsDestiny, [P2, P3]},
  P2 ! {migrantsDestiny, [P3, P4]},
  P3 ! {migrantsDestiny, [P4, P1]},
  P4 ! {migrantsDestiny, [P1, P2]},

  manager:start([P1, P2, P3, P4], Profiler).

r5(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(10, 50, 5, 50, 256, 128),
  Profiler ! {configuration, Conf, 8},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  P2 = poolManager:start(tb2, Pop, Conf, Profiler),
  P3 = poolManager:start(tb3, Pop, Conf, Profiler),
  P4 = poolManager:start(tb4, Pop, Conf, Profiler),
  P5 = poolManager:start(tb5, Pop, Conf, Profiler),
  P6 = poolManager:start(tb6, Pop, Conf, Profiler),
  P7 = poolManager:start(tb7, Pop, Conf, Profiler),
  P8 = poolManager:start(tb8, Pop, Conf, Profiler),

  P1 ! {migrantsDestiny, [P2, P3]},
  P2 ! {migrantsDestiny, [P3, P4]},
  P3 ! {migrantsDestiny, [P4, P5]},
  P4 ! {migrantsDestiny, [P5, P6]},
  P5 ! {migrantsDestiny, [P6, P7]},
  P6 ! {migrantsDestiny, [P7, P8]},
  P7 ! {migrantsDestiny, [P8, P1]},
  P8 ! {migrantsDestiny, [P1, P2]},

  manager:start([P1, P2, P3, P4, P5, P6, P7, P8], Profiler).

r6(Profiler) ->
  {Pop, Conf} = configBuilder:createExperimentConfig(20, 50, 10, 50, 256, 128),
  Profiler ! {configuration, Conf, 8},
  P1 = poolManager:start(tb1, Pop, Conf, Profiler),
  P2 = poolManager:start(tb2, Pop, Conf, Profiler),
  P3 = poolManager:start(tb3, Pop, Conf, Profiler),
  P4 = poolManager:start(tb4, Pop, Conf, Profiler),
  P5 = poolManager:start(tb5, Pop, Conf, Profiler),
  P6 = poolManager:start(tb6, Pop, Conf, Profiler),
  P7 = poolManager:start(tb7, Pop, Conf, Profiler),
  P8 = poolManager:start(tb8, Pop, Conf, Profiler),

  P1 ! {migrantsDestiny, [P2, P3]},
  P2 ! {migrantsDestiny, [P3, P4]},
  P3 ! {migrantsDestiny, [P4, P5]},
  P4 ! {migrantsDestiny, [P5, P6]},
  P5 ! {migrantsDestiny, [P6, P7]},
  P6 ! {migrantsDestiny, [P7, P8]},
  P7 ! {migrantsDestiny, [P8, P1]},
  P8 ! {migrantsDestiny, [P1, P2]},

  manager:start([P1, P2, P3, P4, P5, P6, P7, P8], Profiler).