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

-include("../include/mtypes.hrl").

-compile(export_all).


start() ->
    register(manager, spawn(manager, init, [])).

init() ->
    loop(null, null, null, []).

loop(Conf, GeneralConf, Pool, Clients) ->
    receive

        configurate ->
            Conf1 = configBuilder:createExperimentConfig(512, 32),
            NGeneralConf = configBuilder:createGeneralConfiguration(),
            loop(Conf1, NGeneralConf, Pool, Clients);

        createPool ->
            Pool1 = spawn(GeneralConf#configArchGA.poolModuleName, init, [Conf, true]),
            loop(Conf, GeneralConf, Pool1, Clients);

        createClients ->
            N = GeneralConf#configArchGA.clientsCount,
            L1 = lists:seq(1, N),
            NClients = lists:map(fun(_) ->
                                         spawn(GeneralConf#configArchGA.clientModuleName, init, [Pool, Conf, GeneralConf#configArchGA.clientsCapacity])
                                 end, L1),
            loop(Conf, GeneralConf, Pool, NClients);

        initEvolution ->
            lists:foreach(fun(C) ->
                                  C ! initEvolution
                          end, Clients),
            loop(Conf, GeneralConf, Pool, Clients);

        finalize ->
            Pool ! finalize,
            lists:foreach(fun(C) ->
                                  C ! finalize
                          end, Clients),
            ok
    end.
