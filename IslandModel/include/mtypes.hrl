-compile(export_all).

-record(imodelGA,
{population, %% Population of the each island
  evaluate, %% fitness function
  selectParents, %% function to select de parents between the population in each iteration
  recombination, %%
  mutation, %% operator to select de mutants
  selectNewPopulation, %% function to obtain the new individuals por the nex iteration
  mGap, %% %%  {prob, Vp} or {cant, C}
  mRate, %% %% {percent, P} or {value, V}
  selMigrater, %% function to select migrants individuals
  selReplacement, %% function to select the individuals that be replaced
  terminationCondition, %% function to determinate when to stop
  doWhenEnd = fun(_, _) -> ok end, %% callback to trigger when end the evolution
  doWhenIterate = fun(_, _) -> ok end, %% callback to trigger when do an iteration
  doWhenMigrate = fun(_) -> ok end %% callback to trigger when do a migration
}).
