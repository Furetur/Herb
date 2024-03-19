#! /bin/bash
set -e

export TEST_SUITE_ROOT=test/exec
export COMPILER_BINARY=./_build/default/bin/herbc.exe

N_PARALLEL_WORKERS=6

dune runtest
# requires the `pytest-xdist` package
pytest -n $N_PARALLEL_WORKERS .
