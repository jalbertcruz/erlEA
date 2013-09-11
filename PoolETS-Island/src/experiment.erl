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

r2(Profiler, Manager) ->
  Conf = #confIsland{
    evaluatorsCount = problem:evaluatorsCount(),
    evaluatorsCapacity = problem:evaluatorsCapacity(),
    reproducersCount = problem:reproducersCount(),
    reproducersCapacity = problem:reproducersCapacity(),
    manager = Manager,
    profiler = Profiler
  },

  Profiler ! {configuration, Conf, 2},

  P1 = poolManager:start(),
  P2 = poolManager:start(),

  IslandManager = islandManager:start(),

  PoolConf = #poolManager{
    evaluatorsCount = Conf#confIsland.evaluatorsCount,
    evaluatorsCapacity = Conf#confIsland.evaluatorsCapacity,
    reproducersCount = Conf#confIsland.reproducersCount,
    reproducersCapacity = Conf#confIsland.reproducersCapacity,
    manager = IslandManager,
    profiler = Conf#confIsland.profiler,
    tableName = tb1
  },

  P1 ! {init, PoolConf, problem:genInitPop()},
  P2 ! {init, PoolConf#poolManager{tableName = tb2}, problem:genInitPop()},

  P1 ! {migrantsDestination, [P2]},
  P2 ! {migrantsDestination, [P1]},

  Pools = [P1, P2],

%%   io:format("IslandManager ! {init~n", []),
  IslandManager ! {init, #islandManager{pools = Pools, profiler = Profiler, manager = Manager}},

  PoolsCount = length(Pools),
  Cociente = problem:evaluations() div PoolsCount,
  Resto = problem:evaluations() rem PoolsCount,

  {Primeros, Ultimos} = lists:split(Resto, Pools),

  lists:foreach(fun(P) -> P ! {initEvaluations, Cociente + 1} end, Primeros),

  lists:foreach(fun(P) -> P ! {initEvaluations, Cociente} end, Ultimos),

  IslandManager ! start.


