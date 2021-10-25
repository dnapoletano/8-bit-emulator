cmake_minimum_required(VERSION 3.0)
project(6502Emulator LANGUAGES CXX)
set(CMAKE_MAKE_PROGRAM "Makefile")

message("------------ 6502 Emulator --------------")
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "-g")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
find_package(SDL2 REQUIRED)
if(SDL2_FOUND)
  message(STATUS "SDL found")
endif()

find_package(OpenGL REQUIRED)
if(OPENGL_FOUND)
  message(STATUS "OpenGL found")
endif()

find_package(GLUT REQUIRED)
if(GLUT_FOUND)
  message(STATUS "GLUT found")
endif()

# find_package(Yaml-cpp REQUIRED)

# if(${Yaml-cpp_FOUND})
# message(STATUS "Yaml-correctly found")
# set(YAML_PREFIX "${YAML_CPP_INCLUDE_DIR}/..")
# endif()

# if(NOT YAML_PREFIX)
# message( FATAL_ERROR
# "yaml-cpp path not found automatically. If you have it installed, set it via -DYAML_PREFIX=<path-to-yaml-cpp>.
# If you don't have yaml-cpp installed, please install it and try again" )
# endif()

# # define option to use openmpi compiler
# option (using_mpi "Enable use of the zlib library" ON)
# set (USING_MPI FALSE)
# if((${using_mpi}))
#   find_package(MPI)
#   #find_package(OpenMP)
#   if(${MPI_FOUND})
#   # if(${OpenMP_FOUND})
#     set(USING_MPI TRUE)
#   endif()
# endif()

# configure config.hpp to pass CMake settings to the source code
# configure_file (
#    ${PROJECT_SOURCE_DIR}/include/config.hpp.in
#    ${PROJECT_SOURCE_DIR}/include/config.hpp
# )

add_subdirectory(src)
message("------------   End Configure    --------------")
