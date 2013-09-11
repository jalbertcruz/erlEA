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

-module(problem).
-author("jalbertcruz@gmail.com").

-compile(export_all).

dispatcher() ->
  problemName().

terminationCondition() ->
%%   fitnessTerminationCondition.
  cantEvalsTerminationCondition.

problemName() ->
  maxOnes.
%%     maxSAT.

function(Ind) ->
  apply(dispatcher(), function, [Ind]).

fitnessTerminationCondition(Ind, Fit) ->
  apply(dispatcher(), fitnessTerminationCondition, [Ind, Fit]).

genInitPop() ->
  [genInd() || _ <- lists:seq(1, popSize())].

genInd() ->
  apply(dispatcher(), genInd, []).

evaluatorsCount() ->
  apply(dispatcher(), evaluatorsCount, []).

evaluatorsCapacity() ->
  apply(dispatcher(), evaluatorsCapacity, []).

reproducersCount() ->
  apply(dispatcher(), reproducersCount, []).

reproducersCapacity() ->
  apply(dispatcher(), reproducersCapacity, []).

changeGen(G) ->
  apply(dispatcher(), changeGen, [G]).

evaluations() ->
  apply(dispatcher(), evaluations, []).

popSize() ->
  apply(dispatcher(), popSize, []).

chromosomeSize() ->
  apply(dispatcher(), chromosomeSize, []).