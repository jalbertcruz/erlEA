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
-module(model).

-compile(export_all).

%% Input:
%%	Count -> Number of individuals to be replaced, Population -> Idem, Fit -> Fitness function
%% Returns:
%%	A tuple {Rep, NoRep} -> Rep subset of Population to be replaced, NoRep -> the rest of the Population.
selReplacement(Count, Population, Fit) ->
    Tups = [{X, Fit(X)} || X <- Population],
    TSort = lists:sort(fun({_, V1}, {_, V2}) -> V1 < V2 end, Tups),
    lists:split(Count, TSort).


%% Size most be even
genInitPop(PopulationSize, NChromosomes) ->
    [genInd(NChromosomes) || _ <- lists:seq(1, PopulationSize)].

genInd(N) ->
    [random:uniform(2) - 1 || _ <- lists:seq(1, N)].

%% Input: 
%%    list with a population and a fitness function.
%%
%%  Returns a tuple: {LParents, PNu}, LParents is a list [{Parent1, Parent2}], 
%%  each pair is a selection of parents for beeing combine, PNu is the sublist of individuals
%%  not selected.
parentsSelector(Pop, Fit) ->
    Tups = [{X, Fit(X)} || X <- Pop],
    TSort = lists:sort(fun({_, V1}, {_, V2}) -> V1 > V2 end, Tups),
    {P1, PNu} = lists:split(length(Pop) div 2, TSort),
    P2 = lists:reverse(P1),
    {lists:zip(P1, P2), PNu}.

%% Mutate an individual
mutateInd(S) ->
    MutationPoint = random:uniform(length(S) - 1),
    {Pre, [X | Post]} = lists:split(MutationPoint - 1, S),
    P = probability(1),
    Ngen = if P -> if X == 1 -> 0; true -> 1 end; true -> X end,
    Pre ++ [Ngen | Post].

%% Mutate a population
mutate(Population) ->
    [mutateInd(I) || I <- Population].

recombine(Parent1, Parent2, Pivote1, Pivote2) ->
    {Frag1, _} = lists:split(Pivote1, Parent1),
    {_, Frag3} = lists:split(Pivote2, Parent1),
    Frag2 = lists:sublist(Parent2, Pivote1, Pivote2 - Pivote1),
    Frag1 ++ Frag2 ++ Frag3.

%% Input: A pair of individuals for be crossed
%%   It's use tow point crossover
%% Returns a new individual.
recombine({P1, P2}) ->
    Pivote1 = random:uniform(length(P1) - 1),
    Pivote2 = Pivote1 + random:uniform(length(P1) - Pivote1),
    recombine(P1, P2, Pivote1, Pivote2).

maxOnes(L) ->
    length(lists:filter(fun(X) -> X =:= 1 end, L)).

%% A simple probability function that returns true with 
%% a probability of 1/2^X
probability(X) -> probability(X, X).
probability(_, 0) -> true;
probability(X, A) ->
    N1 = random:uniform(),
    N2 = random:uniform(),
    case N1 > N2 of
        true -> probability(X, A - 1);
        false -> false
    end.


%% Input:
%%	PopulationMutated -> [{IndM, FitIndM}], IndM - Individuals mutated, FitIndM - Fitness of the individual;
%%	Parents -> [{{Ind, FitInd}, {Ind, FitInd}}], Pairs of individuals and fitness selected for recombination;
%%	IndNoSelected -> [{Ind, FitInd}], Individuals and fitness not selected for recombination.
%% Returns: A list [{Ind, Fit}] with the population for the new generation and the corresponding fitness.
selectNewPopulation(PopulationMutated, {Parents, IndNoSelected}) ->
    Length = length(Parents) + length(IndNoSelected),
    Lpm = length(PopulationMutated),
    Missing = Length - Lpm,
    BParents = Missing =< length(Parents),
    LParents = [Ps || {Ps, _} <- Parents],
    if
        BParents ->
            {SelP, _} = lists:split(Missing, LParents),
            SelP;
        true -> {SelNP, _} = lists:split(Missing - length(Parents), IndNoSelected),
                LParents ++ SelNP
    end ++ PopulationMutated.

selectIndividuals(Population, N) ->
    lists:sublist(Population, 1, N).

replaceIndividuals(Population, NewIndividuals) ->
    NPop = lists:sublist(Population, 1, length(Population) - length(NewIndividuals)),
    lists:append(NPop, NewIndividuals).


%% Input:
%%   Population: [Ind] -- list with a population.
%%   Cant: int -- Amount.
%%  Returns a list: [[Ind]] -- With Cant subpopulations.
%%
dividePopulation(Population, Cant) ->
   % io:format("population length: ~p cantidad~p~n", [length(Population), Cant]),
    N = length(Population) div Cant,
    TIndexes = lists:seq(1, length(Population), N),
    RTail = lists:map(fun(Ind) ->
                              lists:sublist(Population, Ind, N)
                      end, TIndexes),
    RTail.
