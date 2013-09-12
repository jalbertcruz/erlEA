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

takeAndMapWhile(_, [], Accu) ->
  {false, Accu};

takeAndMapWhile(PredMap, [Head | Rest], Accu) ->
  {Follow, ResPred} = PredMap(Head),
  Result = [ResPred | Accu],
  if Follow ->
    takeAndMapWhile(PredMap, Rest, Result);
    true ->
      {true, Result}
  end.

evaluate(Sels) ->
  L = length(Sels),
  if
    L < 1 ->
      {false, {false, null}};

    true ->
      Result =
        takeAndMapWhile(
          fun(Ind) ->
            Fit = problem:function(Ind),
            Result = {Ind, Fit},
            TerminationCondition = problem:terminationCondition(),
            if
              TerminationCondition == fitnessTerminationCondition ->
                RfitnessTerminationCondition = problem:fitnessTerminationCondition(Ind, Fit),
                if RfitnessTerminationCondition ->
                  {false, Result};
                  true ->
                    {true, Result}
                end;
              true ->
                {true, Result}
            end
          end, Sels, []),
      {true, Result}
  end.

loop(D) ->
  receive

    {init, PManager, PProfiler} ->
      loop(#evaluator{manager = PManager, profiler = PProfiler});

    {evaluate, Table, N} ->
      Res = ets:select(Table,
        ets:fun2ms(fun({Ind, _, State}) when State == 1 -> Ind end),
        N), % N individuals in state 1
      case Res of
        {Sels, _} ->
          {R, {Founded, NSels}} = evaluate(Sels),
          if
            Founded ->
              [{Ind, Fit} | _] = NSels,
              D#evaluator.manager ! {solutionReachedbyEvaluator, {Ind, Fit}, self()}    ;
            true ->
              ok
          end,
          if
            R ->
              PNSels = [{Ind, Fit, 2} || {Ind, Fit} <- NSels],

              D#evaluator.manager ! {add2Pool, PNSels},
              D#evaluator.manager ! {evalDone, self(), length(NSels)};

            true -> ok
          end;

        '$end_of_table' ->
          D#evaluator.manager ! {evalEmpthyPool, self()}

      end,

      loop(D);

    finalize ->
      D#evaluator.manager ! {evaluatorFinalized, self()},
      ok

  end.
