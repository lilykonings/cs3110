#!/bin/sh

if [ -z "$1" ]; then
  echo "usage: $0 <team-name>"
else
  ocamlc -o "$1" -I +threads -I ../game -I ../shared unix.cma threads.cma str.cma \
  ../shared/thread_pool.mli ../shared/thread_pool.ml ../shared/connection.mli \
  ../shared/connection.ml ../shared/constants.ml ../shared/definitions.ml \
  ../shared/util.mli ../shared/util.ml team.ml "$1.ml"
fi

