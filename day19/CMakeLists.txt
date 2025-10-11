cmake_minimum_required(VERSION 3.10)
project(day19)

# If using GCC, need at least GCC 14, for C++23 deducing this feature.
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED True)
# Also need Boost libraries: algorithm, lexical_cast.
find_package(Boost 1.83 REQUIRED COMPONENTS program_options)

add_compile_options(-Wall -Werror)

add_executable(day19 day19.cpp)
target_link_libraries(day19 Boost::program_options)
