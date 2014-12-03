#!/bin/sh

OUTPUT_EXEC=game

ocamlc -o $OUTPUT_EXEC -I +threads -I ../shared unix.cma threads.cma str.cma \
  ../shared/thread_pool.mli ../shared/thread_pool.ml ../shared/connection.mli \
  ../shared/connection.ml ../shared/constants.ml ../shared/definitions.ml \
  ../shared/util.mli ../shared/util.ml initialization.mli initialization.ml \
  netgraphics.mli netgraphics.ml game.mli game.ml server.ml
