# PS 5 #

## Getting Started ##
Before you do anything, please update your tools:

    bash update-tools.sh

To help you get oriented, here is a brief summary of the files in the release:

- The `map_reduce` folder contains all of the infrastructure code
    - The `MapReduce` module defines the interface between the apps and the
      infrastructure.
    - `RemoteController` and `Worker` are the modules that you have to
      implement.
    - `LocalController` contains a working non-distributed implementation of
      the `Controller` interface. It makes use of `Combiner` for the combine
      phase.
- The `apps` folder contains the apps and the utilities that they use.
- The `async` folder contains the stubs for the async warmup exercises.

## Running Apps ##
    cs3110 compile map_reduce/controllerMain.ml
    cs3110 run map_reduce/controllerMain.ml -- -local wc          apps/word_count/data/badger.txt
    cs3110 run map_reduce/controllerMain.ml -- -local composition apps/relation_composition/data/R.txt apps/relation_composition/data/S.txt
    cs3110 run map_reduce/controllerMain.ml -- -local friends     apps/common_friends/data/graph.txt
