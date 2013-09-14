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

-module(maxSAT).
-author("jalbertcruz@gmail.com").

-compile(export_all).

readClauses(IoDevice, Acc) ->
  {ok, L} = file:read_line(IoDevice),
  Base = string:str(L, "%"),
  if Base == 0 ->
    R = string:tokens(string:strip(L), " "),
    IntValues = lists:map(fun erlang:list_to_integer/1, lists:sublist(R, length(R) - 1)),
    Res = lists:map(
      fun(I) ->
        if
          I < 0 ->
            {false, -I};
          true ->
            {true, I}
        end
      end, IntValues),
    readClauses(IoDevice, [Res | Acc]);
    true ->
      Acc
  end.

init() ->
  {ok, IoDevice} = file:open("../problems/uf100-01.cnf", [{read_ahead, 100}]),
  [file:read_line(IoDevice) || _ <- lists:seq(1, 5)],
  {ok, L} = file:read_line(IoDevice),
  R1 = string:tokens(string:strip(L), " "),
  ClauseLength = list_to_integer(lists:nth(length(R1) - 1, R1)),
  file:read_line(IoDevice),
  {ok, L2} = file:read_line(IoDevice),
  R2 = string:tokens(string:strip(L2), " "),
  VarsCount = list_to_integer(lists:nth(length(R1) - 3, R2)),
  ClausesCount = list_to_integer(lists:nth(length(R1) - 2, R2)),
  Clauses = readClauses(IoDevice, []),
  file:close(IoDevice),
  ets:new(maxSAT, [named_table, set, public]),
  ets:insert(maxSAT, [{clauseLength, ClauseLength}, {varsCount, VarsCount},
    {clausesCount, ClausesCount}, {clauses, Clauses}]).

finalize() -> ets:delete(maxSAT).

function(Ind) ->
  Clauses = ets:lookup_element(maxSAT, clauses, 2),
  length(lists:filter(
    fun(C) ->
      lists:any(
        fun({Sign, Index}) -> lists:nth(Index, Ind) == Sign
        end, C
      )
    end, Clauses
  )).

fitnessTerminationCondition(Ind, Fit) -> Fit > 395.

genInd() ->
  [random:uniform(200) rem 2 == 0 || _ <- lists:seq(1, chromosomeSize())].

evaluatorsCount() -> 25.

reproducersCount() -> 10.

evaluatorsCapacity() -> 50.

reproducersCapacity() -> 50.

changeGen(G) -> not G.

evaluations() -> 5000.

popSize() -> 1024.

chromosomeSize() -> ets:lookup_element(maxSAT, varsCount, 2).