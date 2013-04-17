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
-module(e).
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-import(model, [genInitPop/2]).
-compile(export_all).

r() ->
  profiler:start(),
  {Pop, Conf} = configBuilder:createExperimentConfig(),
  poolManager:start(tb, Pop, Conf).