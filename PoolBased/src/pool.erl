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
-module(pool).

-include("../include/mtypes.hrl").

-compile(export_all).


init(Clients, IM) ->
    loop(Clients, IM).

loop(Clients, IM) ->
    receive

        {configPool, NIM} ->
            loop(Clients, NIM);

        {register2me, Pid} ->
            loop([Pid | Clients], IM);

        {initEvolution, NPopulation} ->
            DividePopulation = IM#imodelGA.dividePopulation,
            Parts = DividePopulation(NPopulation, length(Clients)),
            Pairs = lists:zip(Clients, Parts),
            lists:foreach(fun({C, P}) ->
                                  C ! {evolve, P}
                          end, Pairs),
            loop(Clients, IM#imodelGA{population = NPopulation});

        {generationEnd, Pid, NewIndividuals} ->
            TerminationCondition = IM#imodelGA.terminationCondition,
            {NTerminateValue, Solution} = TerminationCondition(NewIndividuals),
			TerminateValue = not NTerminateValue,
            if TerminateValue ->
                    ReplaceIndividuals = IM#imodelGA.replaceIndividuals,
                    SelectIndividuals = IM#imodelGA.selectIndividuals,
                    Population = IM#imodelGA.population,
                    NPopulation = ReplaceIndividuals(Population, NewIndividuals),
                    Inds2Send = SelectIndividuals(Population, length(Clients)),
                    Pid ! {evolve, Inds2Send},
                    loop(Clients, IM#imodelGA{population = NPopulation});
               true -> 
                    io:format("Solution reached: ~p~n", [Solution]),
                    loop(Clients, IM)

            end;


        finalize ->
            ok
    end.

