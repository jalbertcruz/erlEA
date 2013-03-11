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

-include("../include/mtypes.hrl").

-import(model, [genInitPop/2]).
-compile(export_all).

condition({I, Eval}, Percent) ->
    L = length(I),
    Eval >= L * Percent / 100.

cond80(Ind) ->
    condition(Ind, 80).

run() ->
    manager:start(),
    manager ! configurate,
    manager ! createPool,
    manager ! createClients,
    manager ! registerClientsOnPool,
    manager ! initEvolution.

r0() ->
    manager:start(),
    manager ! configurate,
    manager ! createPool,
    manager ! createClients.

r1() ->
    manager ! registerClientsOnPool.

a() ->
    manager ! initEvolution.
