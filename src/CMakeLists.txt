add_subdirectory(frontend)

add_executable(compiler main.cpp)
target_link_libraries(compiler frontend)