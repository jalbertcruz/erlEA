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
-module(experiment).

-include("../include/mtypes.hrl").

-import(model, [genInitPop/2]).
-compile(export_all).

condition({I, Eval}, Percent)->
    L = length(I),
    Eval>=L*Percent/100.

cond80(Ind)->
    condition(Ind, 80).

config(ChromosomeSize, NumberOfIterations)->
    {A1,A2,A3} = now(),
    random:seed(A1,A2,A3),
    Size = 512,
    #imodelGA{
      population = genInitPop(Size, ChromosomeSize), %% Population of the each island
      evaluate = fun model:maxOnes/1,   %% fitness function
      selectParents = fun model:parentsSelector/2, %% function to select de parents between the population in each iteration
      recombination = fun model:recombine/1, %% 
      mutation = fun model:mutate/1, %% operator to select de mutants
      selectNewPopulation = fun model:selectNewPopulation/2, %% function to obtain the new individuals por the nex iteration
      mGap = {prob, 0.5},  %%  {prob, Vp} o {cant, C} 
      mRate = {value, 2} , %% {percent, P} o {value, V}
      selMigrater = fun model:selMigrater/2, %% function to select migrants individuals
      selReplacement = fun model:selReplacement/3, %% function to select the individuals that be replaced
      terminationCondition = fun(It, _) -> It =:= NumberOfIterations end %% function to determinate when to stop
     }.
    

run(GraphicalNode)->
    C32 = config(32, 20),
    C64 = config(64, 20),
    C128 = config(128, 20),
    Notif = fun(Id, Pop)-> manager ! {iterate, Id, length( [Ind || {Ind, Eval} <- Pop , cond80({Ind, Eval})] )} end,
    %% Network topology created
    N1 = island:start(C32#imodelGA{doWhenEnd=fun(_, _) -> manager ! finalize end, doWhenIterate=fun(_, Pop) -> Notif(32, Pop) end}, null),
    N5 = island:start(C32, N1),
    N4 = island:start(C32, N5),
    N3 = island:start(C32, N4),
    N2 = island:start(C32, N3),
    N1 ! {chDestiny, N2},
    Terminate = fun()->
			N1 ! fin,
			N2 ! fin,
			N3 ! fin,
			N4 ! fin,
			N5 ! fin,
			manager ! fin
		end,

    StartEvolution = fun()->
			     N1 ! startEvolution,
			     N2 ! startEvolution,
			     N3 ! startEvolution,
			     N4 ! startEvolution,
			     N5 ! startEvolution
		     end,
    ChangeParameters = fun(NPmts, Mark) -> 
			       N1 ! {chPmts, NPmts#imodelGA{doWhenEnd=fun(_, _) -> manager ! finalize end, doWhenIterate = fun(_, Pop) -> Notif(Mark, Pop) end}},
			       N2 ! {chPmts, NPmts},
			       N3 ! {chPmts, NPmts},
			       N4 ! {chPmts, NPmts},
			       N5 ! {chPmts, NPmts}
		       end,
    
    manager:start([ {C64, 64}, {C128, 128} ], StartEvolution, ChangeParameters, GraphicalNode),

    StartEvolution(),
    Terminate.

run()->
    run('java@10.13.13.233').
