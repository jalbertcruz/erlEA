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
%% Este actor tomará individuos y les calculará el fitness, solamente
-module(evaluator).
-author("jalbertcruz@gmail.com").
-include_lib("stdlib/include/ms_transform.hrl").

-compile(export_all).

start(Table, PManager) ->
  spawn(evaluator, init, [Table, PManager]).

init(Table, PManager) ->
  loop(Table, PManager).

loop(Table, PManager) ->
  receive

    {eval, N} ->
      MS = ets:fun2ms(fun({Ind, _, State}) when State == 1 -> Ind end),
      Res = ets:select(Table, MS, N), % N individuals in state 1

      case Res of
        {Sels, _} ->
          lists:foreach(fun(Ind) ->
            F = maxOnes(Ind),
            L = length(Ind),
            if L == F ->
              PManager ! solutionReached;
              true -> ok
            end,
            ets:update_element(Table, Ind, [{2, F}, {3, 2}])
          end, Sels),

          PManager ! {evalDone, self()};

        '$end_of_table' -> PManager ! {evalEmpthyPool, self()}
      end,

      loop(Table, PManager);

    finalize ->
      ok

  end.


maxOnes(L) ->
%io:format("maxOnes: ~p~n", [L]),
  length(lists:filter(fun(X) -> X =:= 1 end, L)).
