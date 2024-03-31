add_subdirectory(frontend)
add_subdirectory(middleend)

add_executable(compiler main.cpp)
target_link_libraries(compiler frontend middleend)

