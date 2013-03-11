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
       clientsCount = 8,
       clientModuleName = client,
       poolModuleName = pool,
       population = model:genInitPop(256, 16)
      }.

createExperimentConfig(PopulationSize, ChromosomeSize) ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    #imodelGA{
       population = model:genInitPop(PopulationSize, ChromosomeSize), %% Population of the each island
       evaluate = fun model:maxOnes/1, %% fitness function
       selectParents = fun model:parentsSelector/2, %% function to select the parents between the populations in each iteration
       recombination = fun model:recombine/1, %% 
       mutation = fun model:mutate/1, %% operator to select the mutants
       selectNewPopulation = fun model:selectNewPopulation/2, %% function to obtain the new individuals for the next iteration
       selReplacement = fun model:selReplacement/3, %% function to select the individuals that be replaced
       terminationCondition = fun(Population) -> 
                                      Pairs = lists:filter(fun({_, Fit})-> Fit =:= ChromosomeSize end, Population),
                                      L = length(Pairs) =:= 0,
                                      if L -> 
                                              io:format(": ~p~n", [Population]),
                                              {false, null};

                                         true -> [{_, F} | _] = Pairs,
                                                 io:format("ENCONTRADOOOOOOOOO: ~p~n", [F]),
                                                 {true, F}
                                      end


                              end, %% function to determinate when to stop
       dividePopulation = fun model:dividePopulation/2,
       replaceIndividuals = fun model:replaceIndividuals/2,
       selectIndividuals = fun model:selectIndividuals/2
      }.

