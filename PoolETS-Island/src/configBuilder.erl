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
chromosomeSize() -> 128. % Chromosome length
popSize() -> 256. % Number of individuals
configGA() -> #configGA{
  evaluatorsCount = 50, %% Amount of evaluators
  evaluatorsCapacity = 50, %% Work capacity of evaluators
  reproducersCount = 10,
  reproducersCapacity = 50
}.

createExperimentConfig() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  Population = genInitPop(popSize(), chromosomeSize()), %% The population: chromosomes with ChromosomeSize components, and PopSize individuals
  {Population, configGA()}.

%% Size most be even
genInitPop(PopSize, ChromosomeSize) ->
  [genInd(ChromosomeSize) || _ <- lists:seq(1, PopSize)].

genInd(N) ->
  [random:uniform(2) - 1 || _ <- lists:seq(1, N)].