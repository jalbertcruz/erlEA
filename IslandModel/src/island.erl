%% 
%% Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
%% Copyright 2011 by José Albert Cruz Almaguer.
%% 
%% This program is licensed to you under the terms of version 3 of the
%% GNU Affero General Public License. This program is distributed WITHOUT
%% ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
%% MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
%% AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
%% 
-module(island).

-compile(export_all).

-include("../include/mtypes.hrl").


start(IM, Destiny)->
    NMGap = createmGap(IM),
    CMig = createmRate(IM),
    NIM = IM#imodelGA{mGap=NMGap, mRate=CMig},
    spawn(island, init, [NIM, Destiny]).

createmRate(IM) ->
    case IM#imodelGA.mRate of
	{percent, P} -> round(P / 100 * length(IM#imodelGA.population));
	{value, V} -> V;
	_ -> error
    end.

createmGap(IM) ->
    case IM#imodelGA.mGap of
	{prob, Vp} -> fun(_) ->
			      R = random:uniform(100),
			      R =< Vp * 100
		      end;
	{cant, C} -> fun(It) -> It rem C =:= 0 end;
	_ -> error
    end.

init(IM, Destiny)->    
    loop(0, IM, Destiny).

loop(It, IM, Destiny) ->
    receive
	
	startEvolution ->
	    self() ! iterate,
	    loop(0, IM, Destiny);

	{chDestiny, NDestiny} ->
	    loop(It, IM, NDestiny);

	{chPmts, NPmts} ->
	    NMGap = createmGap(NPmts),
	    CMig = createmRate(NPmts),
	    NIM = NPmts#imodelGA{mGap=NMGap, mRate=CMig},
	    loop(It, NIM, Destiny);
	
	iterate ->
	    PSelector = IM#imodelGA.selectParents,
	    Ev = IM#imodelGA.evaluate,
	    {Parents, IndNoSelected} = PSelector(IM#imodelGA.population, Ev),
	    Rec = IM#imodelGA.recombination,
	    P2 = [ Rec({I1, I2}) || {{I1, _}, {I2, _}} <- Parents],
	    Mut = IM#imodelGA.mutation,
	    P3 = Mut(P2),
	    PopulationMutated = [ {I, Ev(I)} || I <- P3],
	    SNewP = IM#imodelGA.selectNewPopulation,
	    NPopulationE = SNewP(PopulationMutated, {Parents, IndNoSelected}),
	    NMg = IM#imodelGA.mGap,
	    Res = NMg(It),
	    if 
		Res ->  % If most migrate
		    SelM = IM#imodelGA.selMigrater,
		    PopM = SelM(IM#imodelGA.mRate, NPopulationE),
		    Destiny ! {recev, PopM},
		    NotifyMig = IM#imodelGA.doWhenMigrate,
		    NotifyMig(PopM);
		
		true -> ok % else
	    end,
	    NIM = IM#imodelGA{population = [ E || {E, _} <- NPopulationE ]},
	    TCond = IM#imodelGA.terminationCondition,
	    TCondRes = TCond(It, NIM),
	    if 
		TCondRes -> % If most stop
		    EvolEnd = IM#imodelGA.doWhenEnd,
		    EvolEnd(NIM#imodelGA.population, NIM#imodelGA.evaluate),
		    ok;
		true ->  % else
		    self() ! iterate
	    end,
	    NotifyIt = NIM#imodelGA.doWhenIterate,
	    NotifyIt(It + 1, NPopulationE),
	    loop(It + 1, NIM, Destiny);
	
	{recev, NM} ->
	    SelR = IM#imodelGA.selReplacement,
	    {_, Cont} = SelR(length(NM), IM#imodelGA.population,  IM#imodelGA.evaluate),
	    NIM = IM#imodelGA{population=[ C || { C, _ } <- Cont ]  ++ NM},
	    loop(It, NIM, Destiny);
	
	fin ->
	    ok
    end.      
