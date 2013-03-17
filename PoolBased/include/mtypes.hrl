-compile(export_all).

-record(imodelGA,
        {population, %% Population of the each island
         evaluate, %% fitness function
         selectParents, %% function to select de parents between the population in each iteration
         recombination, %%
         mutation, %% operator to select de mutants
         selectNewPopulation, %% function to obtain the new individuals por the nex iteration
         selReplacement, %% function to select the individuals that be replaced
         terminationCondition, %% function to determinate when to stop
         doWhenEnd = fun(_, _) -> ok end, %% callback to trigger when end the evolution
         doWhenIterate = fun(_, _) -> ok end, %% callback to trigger when do an iteration
         dividePopulation,
         replaceIndividuals,
         selectIndividuals
        }).


-record(configArchGA, {
          clientsCount, %% Amount of clients.
          clientsCapacity, %% Work capacity of clients
          clientModuleName, %% Client's module name.
          poolModuleName, %% Pool's module name.
          population
         }).
