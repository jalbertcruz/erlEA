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

-import(model, [genInitPop/2]).
-compile(export_all).


r2() ->
% 10, 50, 5, 50, 256, 128
  profiler:start(),
  {Pop, Conf} = configBuilder:createExperimentConfig(),
  P1 = poolManager:start(tb1, Pop, Conf),
  P2 = poolManager:start(tb2, Pop, Conf),

  P1 ! {migrantsDestiny, [P2]},
  P2 ! {migrantsDestiny, [P1]},

  manager:start([P1, P2]).


r4() ->
  profiler:start(),
  {Pop, Conf} = configBuilder:createExperimentConfig(),
  P1 = poolManager:start(tb1, Pop, Conf),
  P2 = poolManager:start(tb2, Pop, Conf),
  P3 = poolManager:start(tb3, Pop, Conf),
  P4 = poolManager:start(tb4, Pop, Conf),

  P1 ! {migrantsDestiny, [P2, P3]},
  P2 ! {migrantsDestiny, [P3, P4]},
  P3 ! {migrantsDestiny, [P4, P1]},
  P4 ! {migrantsDestiny, [P1, P2]},

  manager:start([P1, P2, P3, P4]).



r8() ->
  profiler:start(),
  {Pop, Conf} = configBuilder:createExperimentConfig(),
  P1 = poolManager:start(tb1, Pop, Conf),
  P2 = poolManager:start(tb2, Pop, Conf),
  P3 = poolManager:start(tb3, Pop, Conf),
  P4 = poolManager:start(tb4, Pop, Conf),
  P5 = poolManager:start(tb5, Pop, Conf),
  P6 = poolManager:start(tb6, Pop, Conf),
  P7 = poolManager:start(tb7, Pop, Conf),
  P8 = poolManager:start(tb8, Pop, Conf),

  P1 ! {migrantsDestiny, [P2, P3]},
  P2 ! {migrantsDestiny, [P3, P4]},
  P3 ! {migrantsDestiny, [P4, P5]},
  P4 ! {migrantsDestiny, [P5, P6]},
  P5 ! {migrantsDestiny, [P6, P7]},
  P6 ! {migrantsDestiny, [P7, P8]},
  P7 ! {migrantsDestiny, [P8, P1]},
  P8 ! {migrantsDestiny, [P1, P2]},

  manager:start([P1, P2, P3, P4, P5, P6, P7, P8]).
