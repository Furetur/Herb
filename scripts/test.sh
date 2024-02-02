#! /bin/bash
set -e

export TEST_SUITE_ROOT=test/exec
export COMPILER_BINARY=./_build/default/bin/herbc.exe

dune runtest
pytest .
