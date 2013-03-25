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

%% API
-compile(export_all).

createGeneralConfiguration() ->
  #configArchGA{
      clientModuleName = client,
      poolModuleName = pool,
%% CONFIG
      clientsCount = 50, %% Amount of clients
      clientsCapacity = 20 %% Work capacity of clients
  }.

createExperimentConfig() ->
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
%% CONFIG
  ChromosomeSize = 128, % Chromosome length
  PopSize = 256, % Number of individuals
  #imodelGA{
      population = model:genInitPop(PopSize, ChromosomeSize), %% The population: chromosomes with ChromosomeSize components, and PopSize individuals
      evaluate = fun model:maxOnes/1, %% fitness function
      selectParents = fun model:parentsSelector/2, %% function to select the parents between the populations in each iteration
      recombination = fun model:recombine/1, %%
      mutation = fun model:mutate/1, %% operator to select the mutants
      selectNewPopulation = fun model:selectNewPopulation/2, %% function to obtain the new individuals for the next iteration
      selReplacement = fun model:selReplacement/3, %% function to select the individuals that be replaced
      terminationCondition = fun(Population) ->
        Pairs = lists:filter(fun({_, Fit}) -> Fit =:= ChromosomeSize end, Population),
        L = length(Pairs) =:= 0,
        if L ->
          {false, null};

          true -> [Sol | _] = Pairs,
            {true, Sol}
        end


      end, %% function to determinate when to stop
      dividePopulation = fun model:dividePopulation/2,
      replaceIndividuals = fun model:replaceIndividuals/3,
      selectIndividuals = fun model:selectIndividuals/2
  }.

