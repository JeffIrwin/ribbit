#!/usr/bin/env bash

# On Ubuntu in WSL, I had to manually install zlib before cmake would build
# assimp:
#
#     sudo apt install -y zlib1g-dev
#

set -xe

pushd tools/conv/

cmake --version

#cmake CMakeLists.txt
cmake -S . -B build
cmake --build build # --config Release # config does nothing?

popd

