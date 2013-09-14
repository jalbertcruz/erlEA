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

-module(maxOnes).
-author("jalbertcruz@gmail.com").

-compile(export_all).

init() ->
  ok.

finalize() ->
  ok.

function(Ind) ->
%io:format("maxOnes: ~p~n", [L]),
  length(lists:filter(fun(X) -> X =:= 1 end, Ind)).

fitnessTerminationCondition(Ind, Fit) -> length(Ind) - Fit < 25.

genInd() ->
  [random:uniform(2) - 1 || _ <- lists:seq(1, chromosomeSize())].

evaluatorsCount() -> 25.

reproducersCount() -> 10.

evaluatorsCapacity() -> 50.

reproducersCapacity() -> 50.

changeGen(G) ->
  if
    G == 1 -> 0;
    true -> 1
  end.

evaluations() -> 5000.

popSize() -> 256.

chromosomeSize() -> 128.