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
-author("j").
-author("jalbertcruz@gmail.com").

-include("../include/mtypes.hrl").

-compile(export_all).

start() ->
  Pid = spawn(report, init, []),
  register(report, Pid),
  Pid.

init() ->
  loop([], []).

loop(Results, Instances) ->

  receive

    {experimentEnd, EvolutionDelay, NEmig, Conf, NIslands} ->
      self() ! mkExperiment,
      loop([{EvolutionDelay, NEmig, Conf, NIslands} | Results], Instances) ;

    {session, Funs} ->
      timer:send_after(50, self(), mkExperiment),
      loop(Results, Funs);

    mkExperiment ->
      L = length(Instances),
      if
        L =/= 0 ->
          [{Module, Function, Pmtos} | Rest] = Instances,
          timer:apply_after(20, Module, Function, Pmtos),
          io:format("Doing experiment: ~p, in: ~p~n", [Function, Module]),
          loop(Results, Rest);

        true ->
          io:format("Evolution Delay | Emigrants | Evaluators Count | Reproducers Count | Islands Count | Config~n"),
          lists:foreach(
            fun({EvolutionDelay, NEmig, Conf, NIslands}) ->
              Ec = Conf#configGA.evaluatorsCount,
              Rc = Conf#configGA.reproducersCount,
              io:format(" ~p | ~p | ~p | ~p | ~p | ~p ~n", [EvolutionDelay, NEmig, Ec, Rc, NIslands, Conf]) end,
            lists:reverse(Results)
          )
      end;

    finalize ->
      ok

  end.