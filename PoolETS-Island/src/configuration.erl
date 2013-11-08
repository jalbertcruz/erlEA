-module(configuration).

-include("../include/mtypes.hrl").

-compile(export_all).

gaConfig()-> #gAConfig{
  terminationCondition=cantEvalsTerminationCondition,
  seqOutputFilename="seqResults.csv",
  parallelOutputFilename="parResults.csv",
  evaluatorsCount=25,
  reproducersCount=10,
  evaluatorsCapacity=50,
  reproducersCapacity=50,
  popSize=1024,
  evaluations=50000,
  chromosomeSize=0,
  repetitions=3
}.
