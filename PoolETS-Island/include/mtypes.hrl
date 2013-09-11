-compile(export_all).

-record(configGA, {
  evaluatorsCount, %% Amount of evaluators
  evaluatorsCapacity, %% Work capacity of evaluators
  reproducersCount,
  reproducersCapacity
}).

-record(evaluator, {
  manager,
  profiler
}).

-record(confIsland, {
  evaluatorsCount,
  evaluatorsCapacity,
  reproducersCount,
  reproducersCapacity,
  manager,
  profiler,
  population = []
}).

-record(islandManager, {
  pools,
  profiler,
  manager,
  endEvol,
  numberOfEvals,
  solutions
}).

-record(manager, {
  results,
  profiler,
  instances
}).

-record(poolManager, {
  tableName,
  pmConf,
  active,
  migrantsDestination,
  profiler,
  manager,
  evals,
  reps,
  evaluations,

  evaluatorsCount,
  evaluatorsCapacity,
  reproducersCount,
  reproducersCapacity,

  corridas
}).

-record(profiler, {
  conf,
  manager,
  initEvol,
  nIslands,
  iterations,
  emigrations,
  evolutionDelay,
  numberOfEvals,
  bestSolution
}).

-record(reproducer, {
  manager,
  profiler
}).
