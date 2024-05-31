add_subdirectory(frontend)
add_subdirectory(middleend)
add_subdirectory(optim)
add_subdirectory(backend)

add_executable(compiler main.cpp)
target_link_libraries(compiler frontend middleend optim backend)

