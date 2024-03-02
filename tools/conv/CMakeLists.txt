
cmake_minimum_required(VERSION 3.22)

set(CONV conv)
project(${CONV})

include(FetchContent)
FetchContent_Declare(
	assimp
	GIT_REPOSITORY https://github.com/JeffIrwin/assimp
	GIT_TAG        01231d0e6001f555c81dcfcc6c581fa5797ccac9
)
FetchContent_MakeAvailable(assimp)

add_executable(${CONV} main.cpp)
target_link_libraries(${CONV} assimp)

