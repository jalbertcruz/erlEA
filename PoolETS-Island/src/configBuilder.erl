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
-module(configBuilder).
-include("../include/mtypes.hrl").

-compile(export_all).

%% CONFIG
mkconfigGA(EvaluatorsCount, EvaluatorsCapacity, ReproducersCount, ReproducersCapacity) ->
  #configGA{
    evaluatorsCount = EvaluatorsCount, %% Amount of evaluators
    evaluatorsCapacity = EvaluatorsCapacity, %% Work capacity of evaluators
    reproducersCount = ReproducersCount,
    reproducersCapacity = ReproducersCapacity
  }.

createExperimentConfig(EvaluatorsCount, EvaluatorsCapacity, ReproducersCount, ReproducersCapacity, PopSize, ChromosomeSize) ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  Population = genInitPop(PopSize, ChromosomeSize), %% The population: chromosomes with ChromosomeSize components, and PopSize individuals
  {Population, mkconfigGA(EvaluatorsCount, EvaluatorsCapacity, ReproducersCount, ReproducersCapacity)}.

%% Size most be even
genInitPop(PopSize, ChromosomeSize) ->
  [genInd(ChromosomeSize) || _ <- lists:seq(1, PopSize)].

genInd(N) ->
  [random:uniform(2) - 1 || _ <- lists:seq(1, N)].