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

-module(evaluator).
-author("jalbertcruz@gmail.com").

-include_lib("stdlib/include/ms_transform.hrl").

-include("../include/mtypes.hrl").

-compile(export_all).

start() ->
  spawn(evaluator, init, []).

init() ->
  loop(none).

evaluate(Sels, DoIfFitnessTerminationCondition) ->
  L = length(Sels),
  if
    L < 1 ->
      {false, null};
    true ->
      NSels = lists:map(
        fun(Ind) ->
          Fit = problem:function(Ind),

          TerminationCondition = problem:terminationCondition(),
          if
            TerminationCondition == fitnessTerminationCondition ->
              RfitnessTerminationCondition = problem:fitnessTerminationCondition(Ind, Fit),
              if RfitnessTerminationCondition ->
                DoIfFitnessTerminationCondition(Ind, Fit);
                true -> ok
              end;
            true -> ok
          end,

          {Ind, Fit}

        end, Sels
      ),
      {true, NSels}
  end.

loop(D) ->
  receive

    {init, PManager, PProfiler} ->
      loop(#evaluator{manager = PManager, profiler = PProfiler});

    {evaluate, Table, N} ->
      MS = ets:fun2ms(fun({Ind, _, State}) when State == 1 -> Ind end),
      Res = ets:select(Table, MS, N), % N individuals in state 1
      case Res of
        {Sels, _} ->
          {R, NSels} = evaluate(Sels,
            fun(Ind, Fit) ->
              D#evaluator.manager ! {solutionReachedbyEvaluator, {Ind, Fit}, self()}
            end
          ),

          if
            R ->
%%               lists:foreach(fun({Ind, Fit}) ->
%%                 ets:update_element(Table, Ind, [{2, Fit}, {3, 2}])
%%               end, NSels),
              PNSels = [{Ind, Fit, 2} || {Ind, Fit} <- NSels],

              D#evaluator.manager ! {add2Pool, PNSels},
              D#evaluator.manager ! {evalDone, self(), length(NSels)};

            true -> ok
          end;

        '$end_of_table' ->
          D#evaluator.manager ! {evalEmpthyPool, self()};

        _ ->
          io:format("Info evaluator: ~p~n", [ets:info(Table)])

      end,

      loop(D);

    finalize ->
      D#evaluator.manager ! {evaluatorFinalized, self()},
%%       io:format("Evaluator ended: ~p, ", [self()]),
      ok

  end.
