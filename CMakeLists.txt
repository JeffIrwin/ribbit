
cmake_minimum_required(VERSION 3.22)

set(RIBBIT ribbit)
project(${RIBBIT} Fortran)

#set(SKIP_DOC_GEN FALSE CACHE BOOL)
set(SKIP_DOC_GEN TRUE)

#include(FetchContent)
#FetchContent_Declare(
#	json_fortran
#	GIT_REPOSITORY https://github.com/JeffIrwin/json-fortran
#	GIT_TAG        18ef4c60493b7b9fbf41b48bb8e3970e59b57b53
#)
#FetchContent_MakeAvailable(json_fortran)

#add_subdirectory(${json_fortran_SOURCE_DIR} ${json_fortran_BINARY_DIR})
#add_subdirectory(${json_fortran_SOURCE_DIR})
add_subdirectory(submodules/json-fortran/)

add_executable(${RIBBIT} src/main.f90)
#target_link_libraries(${RIBBIT} assimp)

target_link_libraries(${RIBBIT} jsonfortran-static)
#target_link_libraries ( ${TEST} jsonfortran::jsonfortran-static )
#target_link_libraries ( ${RIBBIT} json_fortran::jsonfortran-static )

#add_dependencies(${RIBBIT} "json-fortran")
#add_dependencies(${RIBBIT} json_fortran::jsonfortran-static)

#target_include_directories(${RIBBIT} PUBLIC submodules/json-fortran)
include_directories(submodules/json-fortran)

##set_property(TARGET your_target PROPERTY LINKER_LANGUAGE Fortran)
##set_property(TARGET ${RIBBIT} PROPERTY LINKER_LANGUAGE Fortran)
#set(CMAKE_CXX_LINK_EXECUTABLE ifx.exe)
#set(CMAKE_Fortran_LINK_EXECUTABLE ifx.exe)
#set(CMAKE_LINK_EXECUTABLE ifx.exe)
#set(CMAKE_EXE_LINK_EXECUTABLE ifx.exe)

#target_link_options(${RIBBIT} PUBLIC "-Qmkl")
##set(CMAKE_EXE_LINKER_FLAGS "-Qmkl")

target_link_libraries(${RIBBIT} mkl_rt)

#add_custom_command(TARGET ${RIBBIT} POST_BUILD
#	COMMAND ${CMAKE_COMMAND}
#	#-E copy_if_different
#	-E copy
#	"C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll"
#	#$<TARGET_FILE_DIR:${RIBBIT}>)
#	#"./build/Debug/")
#	"./build/Debug/libifcoremdd.dll")
#	#$<TARGET_FILE_DIR:MyTest>)

configure_file(
	"C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll"
	"./Debug/"
	COPYONLY)

## TODO: get add_custom_command() working instead of configure_file().
## Configure file copies *everything* even if only 1 dll is missing
file(GLOB files "C:/Program Files (x86)/Intel/oneAPI/mkl/latest/bin/*.dll")
foreach(file ${files})
	configure_file("${file}" "./Debug/" COPYONLY)
endforeach()

#add_custom_command(
#	#TARGET myTarget POST_BUILD
#	TARGET ${RIBBIT} POST_BUILD
#	#COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BINARY_DIR}
#	COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/$<CONFIG>/"
#	#COMMAND "${CMAKE_COMMAND}" -E copy_if_different "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll" "${CMAKE_BINARY_DIR}/$<CONFIG>/"
#	COMMAND "${CMAKE_COMMAND}" -E copy "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll" "${CMAKE_BINARY_DIR}/$<CONFIG>/libifcoremdd.dll"
#)

