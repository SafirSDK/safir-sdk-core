cmake_minimum_required(VERSION 2.8)

project(swre_test CXX)

include($ENV{SAFIR_SDK}/data/build/safir_sdk_core_config.cmake)

find_package(Boost COMPONENTS thread system)

find_package(PythonInterp)

enable_testing()

add_subdirectory(tracer_cpp)
add_subdirectory(tracer_backdoor)
add_subdirectory(swreport_cpp)

add_subdirectory(tracer_dotnet)
add_subdirectory(swreport_dotnet)

if (SAFIR_ADA_SUPPORT)
   add_subdirectory(tracer_ada)
   add_subdirectory(swreport_ada)
endif()

if (SAFIR_JAVA_SUPPORT)
   add_subdirectory(tracer_java)
   add_subdirectory(swreport_java)
endif()
