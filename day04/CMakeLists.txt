cmake_minimum_required(VERSION 3.10)
project(day04)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED True)
find_package(Boost 1.71 REQUIRED COMPONENTS program_options)

add_compile_options(-Wall -Werror)

add_executable(day04 day04.cpp)
target_link_libraries(day04 Boost::program_options)
