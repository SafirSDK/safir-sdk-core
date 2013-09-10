cmake_minimum_required(VERSION 2.8)

project(swre_test CXX)

include($ENV{SAFIR_SDK}/data/build/cmake_settings.txt)

find_package(Boost COMPONENTS thread date_time system)

find_package(PythonInterp)

FIND_SOURCE_ROOT(SOURCE_ROOT)

enable_testing()

add_subdirectory(tracer_cpp)
add_subdirectory(tracer_dotnet)

if (SAFIR_ADA_SUPPORT)
   add_subdirectory(tracer_ada)
endif()

if (SAFIR_JAVA_SUPPORT)
   add_subdirectory(tracer_java)
endif()

#add_subdirectory(stop_orders_at_exit)
