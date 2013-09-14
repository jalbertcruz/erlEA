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
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").
-compile(export_all).

start() ->
  Pid = spawn(manager, init, []),
  Pid.

init() ->
  loop(none).

loop(D) ->

  receive

    {init, MConf} ->
      problem:init(),
      loop(MConf#manager{instances = [], results = []});

    {experimentEnd, ReportData} ->
      TResults = [ReportData | D#manager.results],
%%       io:format("Best fitness: ~p (time -> ~p)~n", [lists:nth(6, ReportData), now()]),
      LInstances = length(D#manager.instances),

      if
        LInstances == 0 ->
          io:format("All ends!~n", []),
          {ok, IODevice} = file:open("../../results/book2013/erlEA/parResults.csv", [write]),
          file:write(IODevice, "EvolutionDelay,NumberOfEvals,Emigrations,EvaluatorsCount,ReproducersCount,IslandsCount,BestSol\n"),
          lists:foreach(
            fun([EvolutionDelay, NEmig, Conf, NIslands, NumberOfEvals, BestSol]) ->
              Ec = Conf#confIsland.evaluatorsCount,
              Rc = Conf#confIsland.reproducersCount,
              io:format(IODevice, "~p,~p,~p,~p,~p,~p,~p ~n", [EvolutionDelay, NumberOfEvals, NEmig, Ec, Rc, NIslands, BestSol])
            end,
            lists:reverse(TResults)
          ),
          file:close(IODevice),
          ok;
        true ->
          self() ! mkExperiment
      end,
      loop(D#manager{results = TResults});

    {session, Funs} ->
      self() ! mkExperiment,
      loop(D#manager{instances = Funs});

    mkExperiment ->
      L = length(D#manager.instances),
      if
        L =/= 0 ->
          [{Module, Function, Pmtos} | Rest] = D#manager.instances,
          io:format("Doing experiment: ~p, in: ~p (time -> ~p)~n", [Function, Module, now()]),
          apply(Module, Function, Pmtos),
          loop(D#manager{instances = Rest});

        true ->
          loop(D)
      end

  end.