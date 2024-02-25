#!/usr/bin/env bash

set -xe

pushd tools/conv/

cmake --version

#cmake CMakeLists.txt
cmake -B build -S .
cmake --build build

popd

