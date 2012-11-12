cmake_minimum_required(VERSION 2.8)

project(blob_performance CXX)

INCLUDE($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

FILE(GLOB headers *.h)
FILE(GLOB sources *.cpp)

ADD_EXECUTABLE(test ${sources} ${headers})
#FIND_PACKAGE(Boost COMPONENTS regex thread system)

if (NOT MSVC)
  TARGET_LINK_LIBRARIES(test
    dots_generated-DotsTest-cpp
    dots_generated-cpp
    dots_cpp
    ${Boost_LIBRARIES})
endif()

