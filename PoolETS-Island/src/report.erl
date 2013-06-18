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

-module(report).
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

start(Profiler) ->
  Pid = spawn(report, init, [Profiler]),
  register(report, Pid),
  Pid.

init(Profiler) ->
  loop([], [], none, Profiler).

loop(Results, Instances, NumberOfExperiments, Profiler) ->

  receive

    {experimentEnd, EvolutionDelay, NEmig, Conf, NIslands, NumberOfEvals} ->
      TResults = [{EvolutionDelay, NEmig, Conf, NIslands, NumberOfEvals} | Results],
      if
        NumberOfExperiments =:= 1 ->
        {ok, IODevice} = file:open("results.csv", [write]),
          file:write(IODevice, "EvolutionDelay,NumberOfEvals,Emigrations,EvaluatorsCount,ReproducersCount,IslandsCount"),
          lists:foreach(
            fun({EvolutionDelay1, NEmig1, Conf1, NIslands1, NumberOfEvals1}) ->
              Ec = Conf#configGA.evaluatorsCount,
              Rc = Conf#configGA.reproducersCount,
              io:format(IODevice,"~p,~p,~p,~p,~p,~p ~n", [EvolutionDelay1, NumberOfEvals1, NEmig1, Ec, Rc, NIslands1]) end,
            lists:reverse(TResults)
          ),
          file:close(IODevice),
          Profiler ! finalize;
        true ->
          loop(TResults, Instances, NumberOfExperiments - 1, Profiler)
      end;

    {session, Funs} ->
      self() ! mkExperiment,
      loop(Results, Funs, length(Funs), Profiler);

    mkExperiment ->
      L = length(Instances),
      if
        L =/= 0 ->
          [{Module, Function, Pmtos} | Rest] = Instances,
          apply(Module, Function, Pmtos),
          io:format("Doing experiment: ~p, in: ~p~n", [Function, Module]),
          loop(Results, Rest, NumberOfExperiments, Profiler);

        true ->
          loop(Results, Instances, NumberOfExperiments, Profiler)
      end

  end.