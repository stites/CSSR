cmake_minimum_required(VERSION 2.8.11)
project(CSSR)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

# The version number.
set(CSSR_VERSION_MAJOR 0)
set(CSSR_VERSION_MINOR 1)

set(SOURCE_FILES
    Main.cpp
    Machine.cpp
    Hash2.cpp
    Hash.cpp
    G_Array.cpp
    ParseTree.cpp
    States.cpp
    AllStates.cpp
    Test.cpp
    TransTable.cpp
    Common.h)

add_executable(CSSR ${SOURCE_FILES})



