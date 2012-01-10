
-compile(export_all).

-record(imodelGA, {population, %% Population of the each island
		   evaluate,   %% fitness function
		   selectParents, %% function to select de parents between the population in each iteration
		   recombination, %% 
		   mutation, %% operator to select de mutants
		   selectNewPopulation, %% function to obtain the new individuals por the nex iteration
		   mGap,  %% %%  {prob, Vp} ó {cant, C}
		   mRate, %% %% {percent, P} ó {value, V}
		   selMigrater, %% function to select migrants individuals
		   selReplacement, %% function to select the individuals that be replaced
		   terminationCondition, %% function to determinate when to stop
		   doWhenEnd= fun(_, _) -> ok end, %% callback for excecute when end the evolution
		   doWhenIterate= fun(_, _) -> ok end, %% callback for excecute when do an iteration
		   doWhenMigrate = fun(_) -> ok end %% callback for excecute when do a migration
}).
