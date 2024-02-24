#!/usr/bin/env bash

set -xe

pushd tools/conv/

cmake --version

cmake CMakeLists.txt
cmake --build .

popd

