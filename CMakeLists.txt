# Adapted from https://github.com/pku-minic/sysy-cmake-template/blob/master/CMakeLists.txt
cmake_minimum_required(VERSION 3.13)
project(compiler)
set(CMAKE_CXX_STANDARD 20)

add_compile_options(-Wall -Wno-register -Wno-format-security)

# If env var "SANITIZE" is set, enable sanitizers
if (DEFINED ENV{SANITIZE})
	set(SANITIZE "$ENV{SANITIZE}")
	message(STATUS "Sanitizers enabled: ${SANITIZE}")
	add_compile_options(-fsanitize=${SANITIZE})
	add_link_options(-fsanitize=${SANITIZE})
endif()

# options about libraries and includes
set(LIB_DIR "$ENV{CDE_LIBRARY_PATH}/native" CACHE STRING "directory of libraries")
set(INC_DIR "$ENV{CDE_INCLUDE_PATH}" CACHE STRING "directory of includes")
message(STATUS "Library directory: ${LIB_DIR}")
message(STATUS "Include directory: ${INC_DIR}")

# project link directories
link_directories(${LIB_DIR})

# project include directories
include_directories(src)
include_directories(${CMAKE_CURRENT_BINARY_DIR})
include_directories(${INC_DIR})

# Let all executable targets go to /build/.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# target_link_libraries(compiler koopa pthread dl)
add_subdirectory(src)
