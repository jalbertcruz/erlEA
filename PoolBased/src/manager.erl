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
            Conf1 = configBuilder:createGeneralConfiguration(),
            NGeneralConf = configBuilder:createExperimentConfig(512, 32),
            loop(Conf1, NGeneralConf, Pool, Clients),
            ok;

        createPool ->
            Pool1 = spawn(Conf#configArchGA.poolModuleName, init, [[], GeneralConf]),
            loop(Conf, GeneralConf, Pool1, Clients),
            ok;

        createClients ->
            N = Conf#configArchGA.clientsCount,
            L1 = lists:seq(1, N),
            NClients = lists:map(fun(_) ->
                                        spawn(Conf#configArchGA.clientModuleName, init, [Pool, GeneralConf])
                                end, L1),
            loop(Conf, GeneralConf, Pool, NClients),
            ok;

        registerClientsOnPool ->
            lists:foreach(fun(C) ->
                                  C ! register
                          end, Clients),
            loop(Conf, GeneralConf, Pool, Clients),
            ok;

        initEvolution ->
		%	io:format("mandando: ~p~n", [Conf#configArchGA.population]),
            Pool ! {initEvolution, Conf#configArchGA.population},
            loop(Conf, GeneralConf, Pool, Clients),
            ok;

        finalize ->
            Pool ! finalize,
            lists:foreach(fun(C) ->
                                  C ! finalize
                          end, Clients),
            ok
    end.
