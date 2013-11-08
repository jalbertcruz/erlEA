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
  GAConfig = configuration:gaConfig(),
  random:seed(A1, A2, A3),

  Profiler = profiler:start(),
  Manager = manager:start(),

  Manager ! {init, #manager{profiler = Profiler}},
  Profiler ! {init, Manager},

  Manager ! {session,
    [{experiment, r2, [Profiler, Manager]} || _ <- lists:seq(1, GAConfig#gAConfig.repetitions)]
  }.