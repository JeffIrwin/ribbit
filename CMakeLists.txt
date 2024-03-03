
cmake_minimum_required(VERSION 3.22)

set(RIBBIT ribbit)
project(${RIBBIT} Fortran)

set(SKIP_DOC_GEN TRUE)
add_subdirectory(submodules/json-fortran/)

add_executable(${RIBBIT} src/main.f90)
target_link_libraries(${RIBBIT} jsonfortran-static)
include_directories(submodules/json-fortran)

target_link_libraries(${RIBBIT} mkl_rt)

configure_file(
	"C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll"
	"./Debug/"
	COPYONLY)

configure_file(
	"C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremd.dll"
	"./Release/"
	COPYONLY)

## TODO: get add_custom_command() working instead of configure_file().
## Configure file copies *everything* even if only 1 dll is missing
file(GLOB files "C:/Program Files (x86)/Intel/oneAPI/mkl/latest/bin/*.dll")
foreach(file ${files})
	configure_file("${file}" "./Debug/"   COPYONLY)
	configure_file("${file}" "./Release/" COPYONLY)
endforeach()

#add_custom_command(TARGET ${RIBBIT} POST_BUILD
#	COMMAND ${CMAKE_COMMAND}
#	#-E copy_if_different
#	-E copy
#	"C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll"
#	#$<TARGET_FILE_DIR:${RIBBIT}>)
#	#"./build/Debug/")
#	"./build/Debug/libifcoremdd.dll")
#	#$<TARGET_FILE_DIR:MyTest>)
#add_custom_command(
#	#TARGET myTarget POST_BUILD
#	TARGET ${RIBBIT} POST_BUILD
#	#COMMAND ${CMAKE_COMMAND} -E make_directory ${PROJECT_BINARY_DIR}
#	COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_BINARY_DIR}/$<CONFIG>/"
#	#COMMAND "${CMAKE_COMMAND}" -E copy_if_different "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll" "${CMAKE_BINARY_DIR}/$<CONFIG>/"
#	COMMAND "${CMAKE_COMMAND}" -E copy "C:/Program Files (x86)/Intel/oneAPI/compiler/latest/bin/libifcoremdd.dll" "${CMAKE_BINARY_DIR}/$<CONFIG>/libifcoremdd.dll"
#)

