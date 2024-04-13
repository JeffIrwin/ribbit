#!/usr/bin/env bash

set -xe

if [[ $# -ge 1 ]] ; then
	arg=$1
else
	arg="inputs/cubes.ribbit"
fi

# TODO: fix this within ribbit, not here
rm -rf scratch/ribbit-1-*.geo

# Debug
time fpm run --compiler ifx --flag "-fpp -qmkl -heap-arrays0 -check noarg_temp_created" --profile debug -- "$arg"

# TODO: release option

