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
-module(client).

-compile(export_all).

-include("../include/mtypes.hrl").


init(Pool, IM, Capacity) ->
    loop(Pool, IM, Capacity).

loop(Pool, IM, Capacity) ->
    receive

        initEvolution ->
            Pool ! {requestWork, self(), Capacity},
            loop(Pool, IM, Capacity);

        {evolve, P, NIndexes} ->
            SelectParents = IM#imodelGA.selectParents,
            Evaluate = IM#imodelGA.evaluate,
            Recombination = IM#imodelGA.recombination,
            Mutation = IM#imodelGA.mutation,
            SelectNewPopulation = IM#imodelGA.selectNewPopulation,

            {Parents, IndNoSelected} = SelectParents(P, Evaluate),
            Population2 = [Recombination({I1, I2}) || {{I1, _}, {I2, _}} <- Parents],
            Population3 = Mutation(Population2),
            PopulationMutated = [{I, Evaluate(I)} || I <- Population3],
            NPopulationExt = SelectNewPopulation(PopulationMutated, {Parents, IndNoSelected}),
            Pool ! {generationEnd, NPopulationExt, NIndexes},
            Pool ! {requestWork, self(), Capacity},
            loop(Pool, IM, Capacity);

        finalize ->
            ok

    end.
