-compile(export_all).

-record(configGA, {
  evaluatorsCount, %% Amount of evaluators
  evaluatorsCapacity, %% Work capacity of evaluators
  reproducersCount,
  reproducersCapacity
}).