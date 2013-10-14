cmake_minimum_required(VERSION 2.8)

project(logging_test CXX)

include($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

find_package(Boost COMPONENTS thread date_time system)

find_package(PythonInterp)

enable_testing()

add_subdirectory(logging_cpp)
add_subdirectory(logging_dotnet)

if (SAFIR_ADA_SUPPORT)
   add_subdirectory(logging_ada)
endif()

if (SAFIR_JAVA_SUPPORT)
   add_subdirectory(logging_java)
endif()



