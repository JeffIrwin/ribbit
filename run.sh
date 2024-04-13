#!/usr/bin/env bash

# Debug
time fpm run --compiler ifx --flag "-fpp -qmkl -heap-arrays0 -check noarg_temp_created" --profile debug -- inputs/cubes.ribbit

